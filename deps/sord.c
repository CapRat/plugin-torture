/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

// C99
#include <assert.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sord.h"



#ifdef __SSE4_2__
#    include <smmintrin.h>
#endif

ZIX_API uint32_t
zix_digest_start(void)
{
#ifdef __SSE4_2__
	return 1;  // CRC32 initial value
#else
	return 5381;  // DJB hash initial value
#endif
}

ZIX_API uint32_t
zix_digest_add(uint32_t hash, const void* const buf, const size_t len)
{
	const uint8_t* str = (const uint8_t*)buf;
#ifdef __SSE4_2__
	// SSE 4.2 CRC32
	for (size_t i = 0; i < (len / sizeof(uint32_t)); ++i) {
		hash = _mm_crc32_u32(hash, *(const uint32_t*)str);
		str += sizeof(uint32_t);
	}
	if (len & sizeof(uint16_t)) {
		hash = _mm_crc32_u16(hash, *(const uint16_t*)str);
		str += sizeof(uint16_t);
	}
	if (len & sizeof(uint8_t)) {
		hash = _mm_crc32_u8(hash, *(const uint8_t*)str);
	}
#else
	// Classic DJB hash
	for (size_t i = 0; i < len; ++i) {
		hash = (hash << 5) + hash + str[i];
	}
#endif
	return hash;
}

/**
   Primes, each slightly less than twice its predecessor, and as far away
   from powers of two as possible.
*/
static const unsigned sizes[] = {
	53, 97, 193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317,
	196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843,
	50331653, 100663319, 201326611, 402653189, 805306457, 1610612741, 0
};

typedef struct ZixHashEntry {
	struct ZixHashEntry* next;  ///< Next entry in bucket
	uint32_t             hash;  ///< Non-modulo hash value
	// Value follows here (access with zix_hash_value)
} ZixHashEntry;

struct ZixHashImpl {
	ZixHashFunc     hash_func;
	ZixEqualFunc    equal_func;
	ZixHashEntry** buckets;
	const unsigned* n_buckets;
	size_t          value_size;
	unsigned        count;
};

static inline void*
zix_hash_value(ZixHashEntry* entry)
{
	return entry + 1;
}

ZIX_API ZixHash*
zix_hash_new(ZixHashFunc  hash_func,
	ZixEqualFunc equal_func,
	size_t       value_size)
{
	ZixHash* hash = (ZixHash*)malloc(sizeof(ZixHash));
	if (hash) {
		hash->hash_func = hash_func;
		hash->equal_func = equal_func;
		hash->n_buckets = &sizes[0];
		hash->value_size = value_size;
		hash->count = 0;
		if (!(hash->buckets = (ZixHashEntry**)calloc(*hash->n_buckets,
			sizeof(ZixHashEntry*)))) {
			free(hash);
			return NULL;
		}
	}
	return hash;
}

ZIX_API void
zix_hash_free(ZixHash* hash)
{
	for (unsigned b = 0; b < *hash->n_buckets; ++b) {
		ZixHashEntry* bucket = hash->buckets[b];
		for (ZixHashEntry* e = bucket; e;) {
			ZixHashEntry* next = e->next;
			free(e);
			e = next;
		}
	}

	free(hash->buckets);
	free(hash);
}

ZIX_API size_t
zix_hash_size(const ZixHash* hash)
{
	return hash->count;
}

static inline void
insert_entry(ZixHashEntry** bucket, ZixHashEntry* entry)
{
	entry->next = *bucket;
	*bucket = entry;
}

static inline ZixStatus
rehash(ZixHash* hash, unsigned new_n_buckets)
{
	ZixHashEntry** new_buckets = (ZixHashEntry**)calloc(
		new_n_buckets, sizeof(ZixHashEntry*));
	if (!new_buckets) {
		return ZIX_STATUS_NO_MEM;
	}

	const unsigned old_n_buckets = *hash->n_buckets;
	for (unsigned b = 0; b < old_n_buckets; ++b) {
		for (ZixHashEntry* e = hash->buckets[b]; e;) {
			ZixHashEntry* const next = e->next;
			const unsigned      h = e->hash % new_n_buckets;
			insert_entry(&new_buckets[h], e);
			e = next;
		}
	}

	free(hash->buckets);
	hash->buckets = new_buckets;

	return ZIX_STATUS_SUCCESS;
}

static inline ZixHashEntry*
find_entry(const ZixHash* hash,
	const void* key,
	const unsigned h,
	const unsigned h_nomod)
{
	for (ZixHashEntry* e = hash->buckets[h]; e; e = e->next) {
		if (e->hash == h_nomod && hash->equal_func(zix_hash_value(e), key)) {
			return e;
		}
	}
	return NULL;
}

ZIX_API const void*
zix_hash_find(const ZixHash* hash, const void* value)
{
	const unsigned h_nomod = hash->hash_func(value);
	const unsigned h = h_nomod % *hash->n_buckets;
	ZixHashEntry* const entry = find_entry(hash, value, h, h_nomod);
	return entry ? zix_hash_value(entry) : 0;
}

ZIX_API ZixStatus
zix_hash_insert(ZixHash* hash, const void* value, const void** inserted)
{
	unsigned h_nomod = hash->hash_func(value);
	unsigned h = h_nomod % *hash->n_buckets;

	ZixHashEntry* elem = find_entry(hash, value, h, h_nomod);
	if (elem) {
		assert(elem->hash == h_nomod);
		if (inserted) {
			*inserted = zix_hash_value(elem);
		}
		return ZIX_STATUS_EXISTS;
	}

	elem = (ZixHashEntry*)malloc(sizeof(ZixHashEntry) + hash->value_size);
	if (!elem) {
		return ZIX_STATUS_NO_MEM;
	}
	elem->next = NULL;
	elem->hash = h_nomod;
	memcpy(elem + 1, value, hash->value_size);

	const unsigned next_n_buckets = *(hash->n_buckets + 1);
	if (next_n_buckets != 0 && (hash->count + 1) >= next_n_buckets) {
		if (!rehash(hash, next_n_buckets)) {
			h = h_nomod % *(++hash->n_buckets);
		}
	}

	insert_entry(&hash->buckets[h], elem);
	++hash->count;
	if (inserted) {
		*inserted = zix_hash_value(elem);
	}
	return ZIX_STATUS_SUCCESS;
}

ZIX_API ZixStatus
zix_hash_remove(ZixHash* hash, const void* value)
{
	const unsigned h_nomod = hash->hash_func(value);
	const unsigned h = h_nomod % *hash->n_buckets;

	ZixHashEntry** next_ptr = &hash->buckets[h];
	for (ZixHashEntry* e = hash->buckets[h]; e; e = e->next) {
		if (h_nomod == e->hash &&
			hash->equal_func(zix_hash_value(e), value)) {
			*next_ptr = e->next;
			free(e);
			return ZIX_STATUS_SUCCESS;
		}
		next_ptr = &e->next;
	}

	if (hash->n_buckets != sizes) {
		const unsigned prev_n_buckets = *(hash->n_buckets - 1);
		if (hash->count - 1 <= prev_n_buckets) {
			if (!rehash(hash, prev_n_buckets)) {
				--hash->n_buckets;
			}
		}
	}

	--hash->count;
	return ZIX_STATUS_NOT_FOUND;
}

ZIX_API void
zix_hash_foreach(ZixHash* hash,
	ZixHashVisitFunc f,
	void* user_data)
{
	for (unsigned b = 0; b < *hash->n_buckets; ++b) {
		ZixHashEntry* bucket = hash->buckets[b];
		for (ZixHashEntry* e = bucket; e; e = e->next) {
			f(zix_hash_value(e), user_data);
		}
	}
}

// #define ZIX_BTREE_DEBUG 1

#ifndef ZIX_BTREE_PAGE_SIZE
#    define ZIX_BTREE_PAGE_SIZE 4096
#endif

#define ZIX_BTREE_NODE_SPACE (ZIX_BTREE_PAGE_SIZE - 2 * sizeof(uint16_t))
#define ZIX_BTREE_LEAF_VALS  ((ZIX_BTREE_NODE_SPACE / sizeof(void*)) - 1)
#define ZIX_BTREE_INODE_VALS (ZIX_BTREE_LEAF_VALS / 2)

struct ZixBTreeImpl {
	ZixBTreeNode* root;
	ZixDestroyFunc destroy;
	ZixComparator  cmp;
	void* cmp_data;
	size_t         size;
	unsigned       height;  ///< Number of levels, i.e. root only has height 1
};

struct ZixBTreeNodeImpl {
	uint16_t      is_leaf;
	uint16_t      n_vals;
	// On 64-bit we rely on some padding here to get page-sized nodes
	void* vals[ZIX_BTREE_INODE_VALS];  // ZIX_BTREE_LEAF_VALS for leaves
	ZixBTreeNode* children[ZIX_BTREE_INODE_VALS + 1];  // Nonexistent for leaves
};

typedef struct {
	ZixBTreeNode* node;
	unsigned      index;
} ZixBTreeIterFrame;

struct ZixBTreeIterImpl {
	unsigned          level;    ///< Current level in stack
	ZixBTreeIterFrame stack[];  ///< Position stack
};

#ifdef ZIX_BTREE_DEBUG

ZIX_PRIVATE void
print_node(const ZixBTreeNode* n, const char* prefix)
{
	printf("%s[", prefix);
	for (uint16_t v = 0; v < n->n_vals; ++v) {
		printf(" %lu", (uintptr_t)n->vals[v]);
	}
	printf(" ]\n");
}

ZIX_PRIVATE void
print_tree(const ZixBTreeNode* parent, const ZixBTreeNode* node, int level)
{
	if (node) {
		if (!parent) {
			printf("TREE {\n");
		}
		for (int i = 0; i < level + 1; ++i) {
			printf("  ");
		}
		print_node(node, "");
		if (!node->is_leaf) {
			for (uint16_t i = 0; i < node->n_vals + 1; ++i) {
				print_tree(node, node->children[i], level + 1);
			}
		}
		if (!parent) {
			printf("}\n");
		}
	}
}

#endif  // ZIX_BTREE_DEBUG

ZIX_PRIVATE ZixBTreeNode*
zix_btree_node_new(const bool leaf)
{
	assert(sizeof(ZixBTreeNode) == ZIX_BTREE_PAGE_SIZE);
	ZixBTreeNode* node = (ZixBTreeNode*)malloc(sizeof(ZixBTreeNode));
	if (node) {
		node->is_leaf = leaf;
		node->n_vals = 0;
	}
	return node;
}

ZIX_API ZixBTree*
zix_btree_new(const ZixComparator  cmp,
	void* const          cmp_data,
	const ZixDestroyFunc destroy)
{
	ZixBTree* t = (ZixBTree*)malloc(sizeof(ZixBTree));
	if (t) {
		t->root = zix_btree_node_new(true);
		t->destroy = destroy;
		t->cmp = cmp;
		t->cmp_data = cmp_data;
		t->size = 0;
		t->height = 1;
		if (!t->root) {
			free(t);
			return NULL;
		}
	}
	return t;
}

ZIX_PRIVATE void
zix_btree_free_rec(ZixBTree* const t, ZixBTreeNode* const n)
{
	if (n) {
		if (t->destroy) {
			for (uint16_t i = 0; i < n->n_vals; ++i) {
				t->destroy(n->vals[i]);
			}
		}
		if (!n->is_leaf) {
			for (uint16_t i = 0; i < n->n_vals + 1; ++i) {
				zix_btree_free_rec(t, n->children[i]);
			}
		}
		free(n);
	}
}

ZIX_API void
zix_btree_free(ZixBTree* const t)
{
	if (t) {
		zix_btree_free_rec(t, t->root);
		free(t);
	}
}

ZIX_API size_t
zix_btree_size(const ZixBTree* const t)
{
	return t->size;
}

ZIX_PRIVATE uint16_t
zix_btree_max_vals(const ZixBTreeNode* const node)
{
	return node->is_leaf ? ZIX_BTREE_LEAF_VALS : ZIX_BTREE_INODE_VALS;
}

ZIX_PRIVATE uint16_t
zix_btree_min_vals(const ZixBTreeNode* const node)
{
	return ((zix_btree_max_vals(node) + 1) / 2) - 1;
}

/** Shift pointers in `array` of length `n` right starting at `i`. */
ZIX_PRIVATE void
zix_btree_ainsert(void** const   array,
	const uint16_t n,
	const uint16_t i,
	void* const    e)
{
	memmove(array + i + 1, array + i, (n - i) * sizeof(e));
	array[i] = e;
}

/** Erase element `i` in `array` of length `n` and return erased element. */
ZIX_PRIVATE void*
zix_btree_aerase(void** const array, const uint16_t n, const uint16_t i)
{
	void* const ret = array[i];
	memmove(array + i, array + i + 1, (n - i) * sizeof(ret));
	return ret;
}

/** Split lhs, the i'th child of `n`, into two nodes. */
ZIX_PRIVATE ZixBTreeNode*
zix_btree_split_child(ZixBTreeNode* const n,
	const uint16_t      i,
	ZixBTreeNode* const lhs)
{
	assert(lhs->n_vals == zix_btree_max_vals(lhs));
	assert(n->n_vals < ZIX_BTREE_INODE_VALS);
	assert(i < n->n_vals + 1);
	assert(n->children[i] == lhs);

	const uint16_t max_n_vals = zix_btree_max_vals(lhs);
	ZixBTreeNode* rhs = zix_btree_node_new(lhs->is_leaf);
	if (!rhs) {
		return NULL;
	}

	// LHS and RHS get roughly half, less the middle value which moves up
	lhs->n_vals = max_n_vals / 2;
	rhs->n_vals = max_n_vals - lhs->n_vals - 1;

	// Copy large half of values from LHS to new RHS node
	memcpy(rhs->vals,
		lhs->vals + lhs->n_vals + 1,
		rhs->n_vals * sizeof(void*));

	// Copy large half of children from LHS to new RHS node
	if (!lhs->is_leaf) {
		memcpy(rhs->children,
			lhs->children + lhs->n_vals + 1,
			(rhs->n_vals + 1) * sizeof(ZixBTreeNode*));
	}

	// Move middle value up to parent
	zix_btree_ainsert(n->vals, n->n_vals, i, lhs->vals[lhs->n_vals]);

	// Insert new RHS node in parent at position i
	zix_btree_ainsert((void**)n->children, ++n->n_vals, i + 1, rhs);

	return rhs;
}

/** Find the first value in `n` that is not less than `e` (lower bound). */
ZIX_PRIVATE uint16_t
zix_btree_node_find(const ZixBTree* const     t,
	const ZixBTreeNode* const n,
	const void* const         e,
	bool* const               equal)
{
	uint16_t first = 0;
	uint16_t len = n->n_vals;
	while (len > 0) {
		const uint16_t half = len >> 1;
		const uint16_t i = first + half;
		const int      cmp = t->cmp(n->vals[i], e, t->cmp_data);
		if (cmp == 0) {
			*equal = true;
			len = half;  // Keep searching for wildcard matches
		}
		else if (cmp < 0) {
			const uint16_t chop = half + 1;
			first += chop;
			len -= chop;
		}
		else {
			len = half;
		}
	}
	assert(!*equal || t->cmp(n->vals[first], e, t->cmp_data) == 0);
	return first;
}

ZIX_API ZixStatus
zix_btree_insert(ZixBTree* const t, void* const e)
{
	ZixBTreeNode* parent = NULL;     // Parent of n
	ZixBTreeNode* n = t->root;  // Current node
	uint16_t      i = 0;        // Index of n in parent
	while (n) {
		if (n->n_vals == zix_btree_max_vals(n)) {
			// Node is full, split to ensure there is space for a leaf split
			if (!parent) {
				// Root is full, grow tree upwards
				if (!(parent = zix_btree_node_new(false))) {
					return ZIX_STATUS_NO_MEM;
				}
				t->root = parent;
				parent->children[0] = n;
				++t->height;
			}

			ZixBTreeNode* const rhs = zix_btree_split_child(parent, i, n);
			if (!rhs) {
				return ZIX_STATUS_NO_MEM;
			}

			const int cmp = t->cmp(parent->vals[i], e, t->cmp_data);
			if (cmp == 0) {
				return ZIX_STATUS_EXISTS;
			}
			else if (cmp < 0) {
				// Move to new RHS
				n = rhs;
				++i;
			}
		}

		assert(!parent || parent->children[i] == n);

		bool equal = false;
		i = zix_btree_node_find(t, n, e, &equal);
		if (equal) {
			return ZIX_STATUS_EXISTS;
		}
		else if (!n->is_leaf) {
			// Descend to child node left of value
			parent = n;
			n = n->children[i];
		}
		else {
			// Insert into internal node
			zix_btree_ainsert(n->vals, n->n_vals++, i, e);
			break;
		}
	}

	++t->size;

	return ZIX_STATUS_SUCCESS;
}

ZIX_PRIVATE ZixBTreeIter*
zix_btree_iter_new(const ZixBTree* const t)
{
	const size_t s = t->height * sizeof(ZixBTreeIterFrame);

	return (ZixBTreeIter*)calloc(1, sizeof(ZixBTreeIter) + s);
}

ZIX_PRIVATE void
zix_btree_iter_set_frame(ZixBTreeIter* const ti,
	ZixBTreeNode* const n,
	const uint16_t      i)
{
	if (ti) {
		ti->stack[ti->level].node = n;
		ti->stack[ti->level].index = i;
	}
}

ZIX_PRIVATE bool
zix_btree_node_is_minimal(ZixBTreeNode* const n)
{
	assert(n->n_vals >= zix_btree_min_vals(n));
	return n->n_vals == zix_btree_min_vals(n);
}

/** Enlarge left child by stealing a value from its right sibling. */
ZIX_PRIVATE ZixBTreeNode*
zix_btree_rotate_left(ZixBTreeNode* const parent, const uint16_t i)
{
	ZixBTreeNode* const lhs = parent->children[i];
	ZixBTreeNode* const rhs = parent->children[i + 1];

	// Move parent value to end of LHS
	lhs->vals[lhs->n_vals++] = parent->vals[i];

	// Move first child pointer from RHS to end of LHS
	if (!lhs->is_leaf) {
		lhs->children[lhs->n_vals] = (ZixBTreeNode*)zix_btree_aerase(
			(void**)rhs->children, rhs->n_vals, 0);
	}

	// Move first value in RHS to parent
	parent->vals[i] = zix_btree_aerase(rhs->vals, --rhs->n_vals, 0);

	return lhs;
}

/** Enlarge right child by stealing a value from its left sibling. */
ZIX_PRIVATE ZixBTreeNode*
zix_btree_rotate_right(ZixBTreeNode* const parent, const uint16_t i)
{
	ZixBTreeNode* const lhs = parent->children[i - 1];
	ZixBTreeNode* const rhs = parent->children[i];

	// Prepend parent value to RHS
	zix_btree_ainsert(rhs->vals, rhs->n_vals++, 0, parent->vals[i - 1]);

	// Move last child pointer from LHS and prepend to RHS
	if (!lhs->is_leaf) {
		zix_btree_ainsert((void**)rhs->children,
			rhs->n_vals,
			0,
			lhs->children[lhs->n_vals]);
	}

	// Move last value from LHS to parent
	parent->vals[i - 1] = lhs->vals[--lhs->n_vals];

	return rhs;
}

/** Move n[i] down, merge the left and right child, return the merged node. */
ZIX_PRIVATE ZixBTreeNode*
zix_btree_merge(ZixBTree* const t, ZixBTreeNode* const n, const uint16_t i)
{
	ZixBTreeNode* const lhs = n->children[i];
	ZixBTreeNode* const rhs = n->children[i + 1];

	assert(zix_btree_node_is_minimal(n->children[i]));
	assert(lhs->n_vals + rhs->n_vals < zix_btree_max_vals(lhs));

	// Move parent value to end of LHS
	lhs->vals[lhs->n_vals++] = zix_btree_aerase(n->vals, n->n_vals, i);

	// Erase corresponding child pointer (to RHS) in parent
	zix_btree_aerase((void**)n->children, n->n_vals, i + 1);

	// Add everything from RHS to end of LHS
	memcpy(lhs->vals + lhs->n_vals, rhs->vals, rhs->n_vals * sizeof(void*));
	if (!lhs->is_leaf) {
		memcpy(lhs->children + lhs->n_vals,
			rhs->children,
			(rhs->n_vals + 1) * sizeof(void*));
	}
	lhs->n_vals += rhs->n_vals;

	if (--n->n_vals == 0) {
		// Root is now empty, replace it with its only child
		assert(n == t->root);
		t->root = lhs;
		free(n);
	}

	free(rhs);
	return lhs;
}

/** Remove and return the min value from the subtree rooted at `n`. */
ZIX_PRIVATE void*
zix_btree_remove_min(ZixBTree* const t, ZixBTreeNode* n)
{
	while (!n->is_leaf) {
		if (zix_btree_node_is_minimal(n->children[0])) {
			// Leftmost child is minimal, must expand
			if (!zix_btree_node_is_minimal(n->children[1])) {
				// Child's right sibling has at least one key to steal
				n = zix_btree_rotate_left(n, 0);
			}
			else {
				// Both child and right sibling are minimal, merge
				n = zix_btree_merge(t, n, 0);
			}
		}
		else {
			n = n->children[0];
		}
	}

	return zix_btree_aerase(n->vals, --n->n_vals, 0);
}

/** Remove and return the max value from the subtree rooted at `n`. */
ZIX_PRIVATE void*
zix_btree_remove_max(ZixBTree* const t, ZixBTreeNode* n)
{
	while (!n->is_leaf) {
		if (zix_btree_node_is_minimal(n->children[n->n_vals])) {
			// Leftmost child is minimal, must expand
			if (!zix_btree_node_is_minimal(n->children[n->n_vals - 1])) {
				// Child's left sibling has at least one key to steal
				n = zix_btree_rotate_right(n, n->n_vals);
			}
			else {
				// Both child and left sibling are minimal, merge
				n = zix_btree_merge(t, n, n->n_vals - 1);
			}
		}
		else {
			n = n->children[n->n_vals];
		}
	}

	return n->vals[--n->n_vals];
}

ZIX_API ZixStatus
zix_btree_remove(ZixBTree* const      t,
	const void* const    e,
	void** const         out,
	ZixBTreeIter** const next)
{
	ZixBTreeNode* n = t->root;
	ZixBTreeIter* ti = NULL;
	const bool    user_iter = next && *next;
	if (next) {
		if (!*next && !(*next = zix_btree_iter_new(t))) {
			return ZIX_STATUS_NO_MEM;
		}
		ti = *next;
		ti->level = 0;
	}

	while (true) {
		/* To remove in a single walk down, the tree is adjusted along the way
		   so that the current node always has at least one more value than the
		   minimum required in general. Thus, there is always room to remove
		   without adjusting on the way back up. */
		assert(n == t->root || !zix_btree_node_is_minimal(n));

		bool           equal = false;
		const uint16_t i = zix_btree_node_find(t, n, e, &equal);
		zix_btree_iter_set_frame(ti, n, i);
		if (n->is_leaf) {
			if (equal) {
				// Found in leaf node
				*out = zix_btree_aerase(n->vals, --n->n_vals, i);
				if (ti && i == n->n_vals) {
					if (i == 0) {
						ti->stack[ti->level = 0].node = NULL;
					}
					else {
						--ti->stack[ti->level].index;
						zix_btree_iter_increment(ti);
					}
				}
				--t->size;
				return ZIX_STATUS_SUCCESS;
			}
			else {
				// Not found in leaf node, or tree
				if (ti && !user_iter) {
					zix_btree_iter_free(ti);
					*next = NULL;
				}
				return ZIX_STATUS_NOT_FOUND;
			}
		}
		else if (equal) {
			// Found in internal node
			if (!zix_btree_node_is_minimal(n->children[i])) {
				// Left child can remove without merge
				*out = n->vals[i];
				n->vals[i] = zix_btree_remove_max(t, n->children[i]);
				--t->size;
				return ZIX_STATUS_SUCCESS;
			}
			else if (!zix_btree_node_is_minimal(n->children[i + 1])) {
				// Right child can remove without merge
				*out = n->vals[i];
				n->vals[i] = zix_btree_remove_min(t, n->children[i + 1]);
				--t->size;
				return ZIX_STATUS_SUCCESS;
			}
			else {
				// Both preceding and succeeding child are minimal
				n = zix_btree_merge(t, n, i);
			}
		}
		else {
			// Not found in internal node, key is in/under children[i]
			if (zix_btree_node_is_minimal(n->children[i])) {
				if (i > 0 && !zix_btree_node_is_minimal(n->children[i - 1])) {
					// Steal a key from child's left sibling
					n = zix_btree_rotate_right(n, i);
				}
				else if (i < n->n_vals &&
					!zix_btree_node_is_minimal(n->children[i + 1])) {
					// Steal a key from child's right sibling
					n = zix_btree_rotate_left(n, i);
				}
				else {
					// Both child's siblings are minimal, merge them
					if (i < n->n_vals) {
						n = zix_btree_merge(t, n, i);
					}
					else {
						n = zix_btree_merge(t, n, i - 1);
						if (ti) {
							--ti->stack[ti->level].index;
						}
					}
				}
			}
			else {
				n = n->children[i];
			}
		}
		if (ti) {
			++ti->level;
		}
	}

	assert(false);  // Not reached
	return ZIX_STATUS_ERROR;
}

ZIX_API ZixStatus
zix_btree_find(const ZixBTree* const t,
	const void* const     e,
	ZixBTreeIter** const  ti)
{
	ZixBTreeNode* n = t->root;
	if (!(*ti = zix_btree_iter_new(t))) {
		return ZIX_STATUS_NO_MEM;
	}

	while (n) {
		bool           equal = false;
		const uint16_t i = zix_btree_node_find(t, n, e, &equal);

		zix_btree_iter_set_frame(*ti, n, i);

		if (equal) {
			return ZIX_STATUS_SUCCESS;
		}
		else if (n->is_leaf) {
			break;
		}
		else {
			++(*ti)->level;
			n = n->children[i];
		}
	}

	zix_btree_iter_free(*ti);
	*ti = NULL;
	return ZIX_STATUS_NOT_FOUND;
}

ZIX_API ZixStatus
zix_btree_lower_bound(const ZixBTree* const t,
	const void* const     e,
	ZixBTreeIter** const  ti)
{
	if (!t) {
		*ti = NULL;
		return ZIX_STATUS_BAD_ARG;
	}

	ZixBTreeNode* n = t->root;
	bool          found = false;
	unsigned      found_level = 0;
	if (!(*ti = zix_btree_iter_new(t))) {
		return ZIX_STATUS_NO_MEM;
	}

	while (n) {
		bool           equal = false;
		const uint16_t i = zix_btree_node_find(t, n, e, &equal);

		zix_btree_iter_set_frame(*ti, n, i);

		if (equal) {
			found_level = (*ti)->level;
			found = true;
		}

		if (n->is_leaf) {
			break;
		}
		else {
			++(*ti)->level;
			n = n->children[i];
			assert(n);
		}
	}

	const ZixBTreeIterFrame* const frame = &(*ti)->stack[(*ti)->level];
	assert(frame->node);
	if (frame->index == frame->node->n_vals) {
		if (found) {
			// Found on a previous level but went too far
			(*ti)->level = found_level;
		}
		else {
			// Reached end (key is greater than everything in tree)
			(*ti)->stack[0].node = NULL;
		}
	}

	return ZIX_STATUS_SUCCESS;
}

ZIX_API void*
zix_btree_get(const ZixBTreeIter* const ti)
{
	const ZixBTreeIterFrame* const frame = &ti->stack[ti->level];
	assert(frame->node);
	assert(frame->index < frame->node->n_vals);
	return frame->node->vals[frame->index];
}

ZIX_API ZixBTreeIter*
zix_btree_begin(const ZixBTree* const t)
{
	ZixBTreeIter* const i = zix_btree_iter_new(t);
	if (!i) {
		return NULL;
	}
	else if (t->size == 0) {
		i->stack[0].node = NULL;
	}
	else {
		ZixBTreeNode* n = t->root;
		i->stack[0].node = n;
		i->stack[0].index = 0;
		while (!n->is_leaf) {
			n = n->children[0];
			++i->level;
			i->stack[i->level].node = n;
			i->stack[i->level].index = 0;
		}
	}
	return i;
}

ZIX_API bool
zix_btree_iter_is_end(const ZixBTreeIter* const i)
{
	return !i || i->stack[0].node == NULL;
}

ZIX_API void
zix_btree_iter_increment(ZixBTreeIter* const i)
{
	ZixBTreeIterFrame* f = &i->stack[i->level];
	if (f->node->is_leaf) {
		// Leaf, move right
		assert(f->index < f->node->n_vals);
		if (++f->index == f->node->n_vals) {
			// Reached end of leaf, move up
			f = &i->stack[i->level];
			while (i->level > 0 && f->index == f->node->n_vals) {
				f = &i->stack[--i->level];
				assert(f->index <= f->node->n_vals);
			}

			if (f->index == f->node->n_vals) {
				// Reached end of tree
				assert(i->level == 0);
				f->node = NULL;
				f->index = 0;
			}
		}
	}
	else {
		// Internal node, move down to next child
		assert(f->index < f->node->n_vals);
		ZixBTreeNode* child = f->node->children[++f->index];

		f = &i->stack[++i->level];
		f->node = child;
		f->index = 0;

		// Move down and left until we hit a leaf
		while (!f->node->is_leaf) {
			child = f->node->children[0];
			f = &i->stack[++i->level];
			f->node = child;
			f->index = 0;
		}
	}
}

ZIX_API void
zix_btree_iter_free(ZixBTreeIter* const i)
{
	free(i);
}


#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
#    define SORD_UNREACHABLE() __builtin_unreachable()
#else
#    define SORD_UNREACHABLE() assert(false)
#endif

/** Resource node metadata */
typedef struct {
	size_t refs_as_obj;  ///< References as a quad object
} SordResourceMetadata;

/** Literal node metadata */
typedef struct {
	SordNode* datatype;  ///< Optional literal data type URI
	char      lang[16];  ///< Optional language tag
} SordLiteralMetadata;

/** Node */
struct SordNodeImpl {
	SerdNode node;  ///< Serd node
	size_t   refs;  ///< Reference count (# of containing quads)
	union {
		SordResourceMetadata res;
		SordLiteralMetadata  lit;
	} meta;
};


#define SORD_LOG(prefix, ...) fprintf(stderr, "[Sord::" prefix "] " __VA_ARGS__)

#ifdef SORD_DEBUG_ITER
#    define SORD_ITER_LOG(...) SORD_LOG("iter", __VA_ARGS__)
#else
#    define SORD_ITER_LOG(...)
#endif
#ifdef SORD_DEBUG_SEARCH
#    define SORD_FIND_LOG(...) SORD_LOG("search", __VA_ARGS__)
#else
#    define SORD_FIND_LOG(...)
#endif
#ifdef SORD_DEBUG_WRITE
#    define SORD_WRITE_LOG(...) SORD_LOG("write", __VA_ARGS__)
#else
#    define SORD_WRITE_LOG(...)
#endif

#define NUM_ORDERS          12
#define STATEMENT_LEN       3
#define TUP_LEN             (STATEMENT_LEN + 1)
#define DEFAULT_ORDER       SPO
#define DEFAULT_GRAPH_ORDER GSPO

#define TUP_FMT         "(%s %s %s %s)"
#define TUP_FMT_ELEM(e) ((e) ? sord_node_get_string(e) : (const uint8_t*)"*")
#define TUP_FMT_ARGS(t) \
	TUP_FMT_ELEM((t)[0]), \
	TUP_FMT_ELEM((t)[1]), \
	TUP_FMT_ELEM((t)[2]), \
	TUP_FMT_ELEM((t)[3])

#define TUP_S 0
#define TUP_P 1
#define TUP_O 2
#define TUP_G 3

/** Triple ordering */
typedef enum {
	SPO,   ///<         Subject,   Predicate, Object
	SOP,   ///<         Subject,   Object,    Predicate
	OPS,   ///<         Object,    Predicate, Subject
	OSP,   ///<         Object,    Subject,   Predicate
	PSO,   ///<         Predicate, Subject,   Object
	POS,   ///<         Predicate, Object,    Subject
	GSPO,  ///< Graph,  Subject,   Predicate, Object
	GSOP,  ///< Graph,  Subject,   Object,    Predicate
	GOPS,  ///< Graph,  Object,    Predicate, Subject
	GOSP,  ///< Graph,  Object,    Subject,   Predicate
	GPSO,  ///< Graph,  Predicate, Subject,   Object
	GPOS   ///< Graph,  Predicate, Object,    Subject
} SordOrder;

#ifdef SORD_DEBUG_SEARCH
/** String name of each ordering (array indexed by SordOrder) */
static const char* const order_names[NUM_ORDERS] = {
	"spo",  "sop",  "ops",  "osp",  "pso",  "pos",
	"gspo", "gsop", "gops", "gosp", "gpso", "gpos"
};
#endif

/**
   Quads of indices for each order, from most to least significant
   (array indexed by SordOrder)
*/
static const int orderings[NUM_ORDERS][TUP_LEN] = {
	{ 0, 1, 2, 3 }, { 0, 2, 1, 3 },  // SPO, SOP
	{ 2, 1, 0, 3 }, { 2, 0, 1, 3 },  // OPS, OSP
	{ 1, 0, 2, 3 }, { 1, 2, 0, 3 },  // PSO, POS
	{ 3, 0, 1, 2 }, { 3, 0, 2, 1 },  // GSPO, GSOP
	{ 3, 2, 1, 0 }, { 3, 2, 0, 1 },  // GOPS, GOSP
	{ 3, 1, 0, 2 }, { 3, 1, 2, 0 }   // GPSO, GPOS
};

/** World */
struct SordWorldImpl {
	ZixHash*      nodes;
	SerdErrorSink error_sink;
	void*         error_handle;
};

/** Store */
struct SordModelImpl {
	SordWorld* world;

	/** Index for each possible triple ordering (may or may not exist).
	 * Each index is a tree of SordQuad with the appropriate ordering.
	 */
	ZixBTree* indices[NUM_ORDERS];

	size_t n_quads;
	size_t n_iters;
};

/** Mode for searching or iteration */
typedef enum {
	ALL,           ///< Iterate over entire store
	SINGLE,        ///< Iteration over a single element (exact search)
	RANGE,         ///< Iterate over range with equal prefix
	FILTER_RANGE,  ///< Iterate over range with equal prefix, filtering
	FILTER_ALL     ///< Iterate to end of store, filtering
} SearchMode;

/** Iterator over some range of a store */
struct SordIterImpl {
	const SordModel* sord;               ///< Model being iterated over
	ZixBTreeIter*    cur;                ///< Current DB cursor
	SordQuad         pat;                ///< Pattern (in ordering order)
	SordOrder        order;              ///< Store order (which index)
	SearchMode       mode;               ///< Iteration mode
	int              n_prefix;           ///< Prefix for RANGE and FILTER_RANGE
	bool             end;                ///< True iff reached end
	bool             skip_graphs;        ///< Iteration should ignore graphs
};

static uint32_t
sord_node_hash(const void* n)
{
	const SordNode* node = (const SordNode*)n;
	uint32_t        hash = zix_digest_start();
	hash = zix_digest_add(hash, node->node.buf, node->node.n_bytes);
	hash = zix_digest_add(hash, &node->node.type, sizeof(node->node.type));
	if (node->node.type == SERD_LITERAL) {
		hash = zix_digest_add(hash, &node->meta.lit, sizeof(node->meta.lit));
	}
	return hash;
}

static bool
sord_node_hash_equal(const void* a, const void* b)
{
	const SordNode* a_node = (const SordNode*)a;
	const SordNode* b_node = (const SordNode*)b;
	return (a_node == b_node)
		|| ((a_node->node.type == b_node->node.type) &&
		    (a_node->node.type != SERD_LITERAL ||
		     (a_node->meta.lit.datatype == b_node->meta.lit.datatype &&
		      !strncmp(a_node->meta.lit.lang,
		               b_node->meta.lit.lang,
		               sizeof(a_node->meta.lit.lang)))) &&
		    (serd_node_equals(&a_node->node, &b_node->node)));
}

static void
error(SordWorld* world, SerdStatus st, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const SerdError e = { st, NULL, 0, 0, fmt, &args };
	if (world->error_sink) {
		world->error_sink(world->error_handle, &e);
	} else {
		fprintf(stderr, "error: ");
		vfprintf(stderr, fmt, args);
	}
	va_end(args);
}

SordWorld*
sord_world_new(void)
{
	SordWorld* world = (SordWorld*)malloc(sizeof(SordWorld));
	world->error_sink   = NULL;
	world->error_handle = NULL;

	world->nodes = zix_hash_new(
		sord_node_hash, sord_node_hash_equal, sizeof(SordNode));

	return world;
}

static void
free_node_entry(void* value, void* user_data)
{
	SordNode* node = (SordNode*)value;
	if (node->node.type == SERD_LITERAL) {
		sord_node_free((SordWorld*)user_data, node->meta.lit.datatype);
	}
	free((uint8_t*)node->node.buf);
}

void
sord_world_free(SordWorld* world)
{
	zix_hash_foreach(world->nodes, free_node_entry, world);
	zix_hash_free(world->nodes);
	free(world);
}

void
sord_world_set_error_sink(SordWorld*    world,
                          SerdErrorSink error_sink,
                          void*         handle)
{
	world->error_sink   = error_sink;
	world->error_handle = handle;
}

/** Compare nodes, considering NULL a wildcard match. */
static inline int
sord_node_compare(const SordNode* a, const SordNode* b)
{
	if (a == b || !a || !b) {
		return 0;  // Exact or wildcard match
	} else if (a->node.type != b->node.type) {
		return a->node.type - b->node.type;
	}

	int cmp = 0;
	switch (a->node.type) {
	case SERD_URI:
	case SERD_BLANK:
		return strcmp((const char*)a->node.buf, (const char*)b->node.buf);
	case SERD_LITERAL:
		cmp = strcmp((const char*)sord_node_get_string(a),
		             (const char*)sord_node_get_string(b));
		if (cmp == 0) {
			// Note: Can't use sord_node_compare here since it does wildcards
			if (!a->meta.lit.datatype || !b->meta.lit.datatype) {
				cmp = a->meta.lit.datatype - b->meta.lit.datatype;
			} else {
				cmp = strcmp((const char*)a->meta.lit.datatype->node.buf,
				             (const char*)b->meta.lit.datatype->node.buf);
			}
		}
		if (cmp == 0) {
			cmp = strcmp(a->meta.lit.lang, b->meta.lit.lang);
		}
	default:
		break;
	}
	return cmp;
}

bool
sord_node_equals(const SordNode* a, const SordNode* b)
{
	return a == b;  // Nodes are interned
}

/** Return true iff IDs are equivalent, or one is a wildcard */
static inline bool
sord_id_match(const SordNode* a, const SordNode* b)
{
	return !a || !b || (a == b);
}

static inline bool
sord_quad_match_inline(const SordQuad x, const SordQuad y)
{
	return sord_id_match(x[0], y[0])
		&& sord_id_match(x[1], y[1])
		&& sord_id_match(x[2], y[2])
		&& sord_id_match(x[3], y[3]);
}

bool
sord_quad_match(const SordQuad x, const SordQuad y)
{
	return sord_quad_match_inline(x, y);
}

/**
   Compare two quad IDs lexicographically.
   NULL IDs (equal to 0) are treated as wildcards, always less than every
   other possible ID, except itself.
*/
static int
sord_quad_compare(const void* x_ptr, const void* y_ptr, void* user_data)
{
	const int* const           ordering = (const int*)user_data;
	const SordNode*const*const x        = (const SordNode*const*)x_ptr;
	const SordNode*const*const y        = (const SordNode*const*)y_ptr;

	for (int i = 0; i < TUP_LEN; ++i) {
		const int idx = ordering[i];
		const int cmp = sord_node_compare(x[idx], y[idx]);
		if (cmp) {
			return cmp;
		}
	}

	return 0;
}

static inline bool
sord_iter_forward(SordIter* iter)
{
	if (!iter->skip_graphs) {
		zix_btree_iter_increment(iter->cur);
		return zix_btree_iter_is_end(iter->cur);
	}

	SordNode**     key     = (SordNode**)zix_btree_get(iter->cur);
	const SordQuad initial = { key[0], key[1], key[2], key[3] };
	zix_btree_iter_increment(iter->cur);
	while (!zix_btree_iter_is_end(iter->cur)) {
		key = (SordNode**)zix_btree_get(iter->cur);
		for (int i = 0; i < 3; ++i) {
			if (key[i] != initial[i]) {
				return false;
			}
		}

		zix_btree_iter_increment(iter->cur);
	}

	return true;
}

/**
   Seek forward as necessary until `iter` points at a match.
   @return true iff iterator reached end of valid range.
*/
static inline bool
sord_iter_seek_match(SordIter* iter)
{
	for (iter->end = true;
	     !zix_btree_iter_is_end(iter->cur);
	     sord_iter_forward(iter)) {
		const SordNode** const key = (const SordNode**)zix_btree_get(iter->cur);
		if (sord_quad_match_inline(key, iter->pat)) {
			return (iter->end = false);
		}
	}
	return true;
}

/**
   Seek forward as necessary until `iter` points at a match, or the prefix
   no longer matches iter->pat.
   @return true iff iterator reached end of valid range.
*/
static inline bool
sord_iter_seek_match_range(SordIter* iter)
{
	assert(!iter->end);

	do {
		const SordNode** key = (const SordNode**)zix_btree_get(iter->cur);

		if (sord_quad_match_inline(key, iter->pat)) {
			return false;  // Found match
		}

		for (int i = 0; i < iter->n_prefix; ++i) {
			const int idx = orderings[iter->order][i];
			if (!sord_id_match(key[idx], iter->pat[idx])) {
				iter->end = true;  // Reached end of valid range
				return true;
			}
		}
	} while (!sord_iter_forward(iter));

	return (iter->end = true);  // Reached end
}

static SordIter*
sord_iter_new(const SordModel* sord, ZixBTreeIter* cur, const SordQuad pat,
              SordOrder order, SearchMode mode, int n_prefix)
{
	SordIter* iter = (SordIter*)malloc(sizeof(SordIter));
	iter->sord        = sord;
	iter->cur         = cur;
	iter->order       = order;
	iter->mode        = mode;
	iter->n_prefix    = n_prefix;
	iter->end         = false;
	iter->skip_graphs = order < GSPO;
	for (int i = 0; i < TUP_LEN; ++i) {
		iter->pat[i] = pat[i];
	}

	switch (iter->mode) {
	case ALL:
	case SINGLE:
	case RANGE:
		assert(
			sord_quad_match_inline((const SordNode**)zix_btree_get(iter->cur),
			                       iter->pat));
		break;
	case FILTER_RANGE:
		sord_iter_seek_match_range(iter);
		break;
	case FILTER_ALL:
		sord_iter_seek_match(iter);
		break;
	}

#ifdef SORD_DEBUG_ITER
	SordQuad value;
	sord_iter_get(iter, value);
	SORD_ITER_LOG("New %p pat=" TUP_FMT " cur=" TUP_FMT " end=%d skip=%d\n",
	              (void*)iter, TUP_FMT_ARGS(pat), TUP_FMT_ARGS(value),
	              iter->end, iter->skip_graphs);
#endif

	++((SordModel*)sord)->n_iters;
	return iter;
}

const SordModel*
sord_iter_get_model(SordIter* iter)
{
	return iter->sord;
}

void
sord_iter_get(const SordIter* iter, SordQuad tup)
{
	SordNode** key = (SordNode**)zix_btree_get(iter->cur);
	for (int i = 0; i < TUP_LEN; ++i) {
		tup[i] = key[i];
	}
}

const SordNode*
sord_iter_get_node(const SordIter* iter, SordQuadIndex index)
{
	return (!sord_iter_end(iter)
	        ? ((SordNode**)zix_btree_get(iter->cur))[index]
	        : NULL);
}

static bool
sord_iter_scan_next(SordIter* iter)
{
	if (iter->end) {
		return true;
	}

	const SordNode** key;
	if (!iter->end) {
		switch (iter->mode) {
		case ALL:
			// At the end if the cursor is (assigned above)
			break;
		case SINGLE:
			iter->end = true;
			SORD_ITER_LOG("%p reached single end\n", (void*)iter);
			break;
		case RANGE:
			SORD_ITER_LOG("%p range next\n", (void*)iter);
			// At the end if the MSNs no longer match
			key = (const SordNode**)zix_btree_get(iter->cur);
			assert(key);
			for (int i = 0; i < iter->n_prefix; ++i) {
				const int idx = orderings[iter->order][i];
				if (!sord_id_match(key[idx], iter->pat[idx])) {
					iter->end = true;
					SORD_ITER_LOG("%p reached non-match end\n", (void*)iter);
					break;
				}
			}
			break;
		case FILTER_RANGE:
			// Seek forward to next match, stopping if prefix changes
			sord_iter_seek_match_range(iter);
			break;
		case FILTER_ALL:
			// Seek forward to next match
			sord_iter_seek_match(iter);
			break;
		}
	} else {
		SORD_ITER_LOG("%p reached index end\n", (void*)iter);
	}

	if (iter->end) {
		SORD_ITER_LOG("%p Reached end\n", (void*)iter);
		return true;
	} else {
#ifdef SORD_DEBUG_ITER
		SordQuad tup;
		sord_iter_get(iter, tup);
		SORD_ITER_LOG("%p Increment to " TUP_FMT "\n",
		              (void*)iter, TUP_FMT_ARGS(tup));
#endif
		return false;
	}
}

bool
sord_iter_next(SordIter* iter)
{
	if (iter->end) {
		return true;
	}

	iter->end = sord_iter_forward(iter);
	return sord_iter_scan_next(iter);
}

bool
sord_iter_end(const SordIter* iter)
{
	return !iter || iter->end;
}

void
sord_iter_free(SordIter* iter)
{
	SORD_ITER_LOG("%p Free\n", (void*)iter);
	if (iter) {
		--((SordModel*)iter->sord)->n_iters;
		zix_btree_iter_free(iter->cur);
		free(iter);
	}
}

/**
   Return true iff `sord` has an index for `order`.
   If `graphs` is true, `order` will be modified to be the
   corresponding order with a G prepended (so G will be the MSN).
*/
static inline bool
sord_has_index(SordModel* model, SordOrder* order, int* n_prefix, bool graphs)
{
	if (graphs) {
		*order     = (SordOrder)(*order + GSPO);
		*n_prefix += 1;
	}

	return model->indices[*order];
}

/**
   Return the best available index for a pattern.
   @param pat Pattern in standard (S P O G) order
   @param mode Set to the (best) iteration mode for iterating over results
   @param n_prefix Set to the length of the range prefix
   (for `mode` == RANGE and `mode` == FILTER_RANGE)
*/
static inline SordOrder
sord_best_index(SordModel*     sord,
                const SordQuad pat,
                SearchMode*    mode,
                int*           n_prefix)
{
	const bool graph_search = (pat[TUP_G] != 0);

	const unsigned sig
		= (pat[0] ? 1 : 0) * 0x100
		+ (pat[1] ? 1 : 0) * 0x010
		+ (pat[2] ? 1 : 0) * 0x001;

	SordOrder good[2] = { (SordOrder)-1, (SordOrder)-1 };

#define PAT_CASE(sig, m, g0, g1, np) \
	case sig: \
		*mode     = m; \
		good[0]   = g0; \
		good[1]   = g1; \
		*n_prefix = np; \
		break

	// Good orderings that don't require filtering
	*mode     = RANGE;
	*n_prefix = 0;
	switch (sig) {
	case 0x000:
		assert(graph_search);
		*mode     = RANGE;
		*n_prefix = 1;
		return DEFAULT_GRAPH_ORDER;
	case 0x111:
		*mode = SINGLE;
		return graph_search ? DEFAULT_GRAPH_ORDER : DEFAULT_ORDER;

		PAT_CASE(0x001, RANGE, OPS, OSP, 1);
		PAT_CASE(0x010, RANGE, POS, PSO, 1);
		PAT_CASE(0x011, RANGE, OPS, POS, 2);
		PAT_CASE(0x100, RANGE, SPO, SOP, 1);
		PAT_CASE(0x101, RANGE, SOP, OSP, 2);
		PAT_CASE(0x110, RANGE, SPO, PSO, 2);
	}

	if (*mode == RANGE) {
		if (sord_has_index(sord, &good[0], n_prefix, graph_search)) {
			return good[0];
		} else if (sord_has_index(sord, &good[1], n_prefix, graph_search)) {
			return good[1];
		}
	}

	// Not so good orderings that require filtering, but can
	// still be constrained to a range
	switch (sig) {
		PAT_CASE(0x011, FILTER_RANGE, OSP, PSO, 1);
		PAT_CASE(0x101, FILTER_RANGE, SPO, OPS, 1);
		// SPO is always present, so 0x110 is never reached here
	default: break;
	}

	if (*mode == FILTER_RANGE) {
		if (sord_has_index(sord, &good[0], n_prefix, graph_search)) {
			return good[0];
		} else if (sord_has_index(sord, &good[1], n_prefix, graph_search)) {
			return good[1];
		}
	}

	if (graph_search) {
		*mode = FILTER_RANGE;
		*n_prefix = 1;
		return DEFAULT_GRAPH_ORDER;
	} else {
		*mode = FILTER_ALL;
		return DEFAULT_ORDER;
	}
}

SordModel*
sord_new(SordWorld* world, unsigned indices, bool graphs)
{
	SordModel* model = (SordModel*)malloc(sizeof(struct SordModelImpl));
	model->world   = world;
	model->n_quads = 0;
	model->n_iters = 0;

	for (unsigned i = 0; i < (NUM_ORDERS / 2); ++i) {
		const int* const ordering   = orderings[i];
		const int* const g_ordering = orderings[i + (NUM_ORDERS / 2)];

		if (indices & (1 << i)) {
			model->indices[i] = zix_btree_new(
				sord_quad_compare, (void*)ordering, NULL);
			if (graphs) {
				model->indices[i + (NUM_ORDERS / 2)] = zix_btree_new(
					sord_quad_compare, (void*)g_ordering, NULL);
			} else {
				model->indices[i + (NUM_ORDERS / 2)] = NULL;
			}
		} else {
			model->indices[i] = NULL;
			model->indices[i + (NUM_ORDERS / 2)] = NULL;
		}
	}

	if (!model->indices[DEFAULT_ORDER]) {
		model->indices[DEFAULT_ORDER] = zix_btree_new(
			sord_quad_compare, (void*)orderings[DEFAULT_ORDER], NULL);
	}
	if (graphs && !model->indices[DEFAULT_GRAPH_ORDER]) {
		model->indices[DEFAULT_GRAPH_ORDER] = zix_btree_new(
			sord_quad_compare, (void*)orderings[DEFAULT_GRAPH_ORDER], NULL);
	}

	return model;
}

static void
sord_node_free_internal(SordWorld* world, SordNode* node)
{
	assert(node->refs == 0);

	// Cache pointer to buffer to free after node removal and destruction
	const uint8_t* const buf = node->node.buf;

	// Remove node from hash (which frees the node)
	if (zix_hash_remove(world->nodes, node)) {
		error(world, SERD_ERR_INTERNAL, "failed to remove node from hash\n");
	}

	// Free buffer
	free((uint8_t*)buf);
}

static void
sord_add_quad_ref(SordModel* model, const SordNode* node, SordQuadIndex i)
{
	if (node) {
		assert(node->refs > 0);
		++((SordNode*)node)->refs;
		if (node->node.type != SERD_LITERAL && i == SORD_OBJECT) {
			++((SordNode*)node)->meta.res.refs_as_obj;
		}
	}
}

static void
sord_drop_quad_ref(SordModel* model, const SordNode* node, SordQuadIndex i)
{
	if (!node) {
		return;
	}

	assert(node->refs > 0);
	if (node->node.type != SERD_LITERAL && i == SORD_OBJECT) {
		assert(node->meta.res.refs_as_obj > 0);
		--((SordNode*)node)->meta.res.refs_as_obj;
	}
	if (--((SordNode*)node)->refs == 0) {
		sord_node_free_internal(sord_get_world(model), (SordNode*)node);
	}
}

void
sord_free(SordModel* model)
{
	if (!model) {
		return;
	}

	// Free nodes
	SordQuad tup;
	SordIter* i = sord_begin(model);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		sord_iter_get(i, tup);
		for (int t = 0; t < TUP_LEN; ++t) {
			sord_drop_quad_ref(model, tup[t], (SordQuadIndex)t);
		}
	}
	sord_iter_free(i);

	// Free quads
	ZixBTreeIter* t = zix_btree_begin(model->indices[DEFAULT_ORDER]);
	for (; !zix_btree_iter_is_end(t); zix_btree_iter_increment(t)) {
		free(zix_btree_get(t));
	}
	zix_btree_iter_free(t);

	// Free indices
	for (unsigned o = 0; o < NUM_ORDERS; ++o) {
		if (model->indices[o]) {
			zix_btree_free(model->indices[o]);
		}
	}

	free(model);
}

SordWorld*
sord_get_world(SordModel* model)
{
	return model->world;
}

size_t
sord_num_quads(const SordModel* model)
{
	return model->n_quads;
}

size_t
sord_num_nodes(const SordWorld* world)
{
	return zix_hash_size(world->nodes);
}

SordIter*
sord_begin(const SordModel* model)
{
	if (sord_num_quads(model) == 0) {
		return NULL;
	} else {
		ZixBTreeIter* cur = zix_btree_begin(model->indices[DEFAULT_ORDER]);
		SordQuad      pat = { 0, 0, 0, 0 };
		return sord_iter_new(model, cur, pat, DEFAULT_ORDER, ALL, 0);
	}
}

SordIter*
sord_find(SordModel* model, const SordQuad pat)
{
	if (!pat[0] && !pat[1] && !pat[2] && !pat[3]) {
		return sord_begin(model);
	}

	SearchMode      mode;
	int             n_prefix;
	const SordOrder index_order = sord_best_index(model, pat, &mode, &n_prefix);

	SORD_FIND_LOG("Find " TUP_FMT "  index=%s  mode=%d  n_prefix=%d\n",
	              TUP_FMT_ARGS(pat), order_names[index_order], mode, n_prefix);

	if (pat[0] && pat[1] && pat[2] && pat[3]) {
		mode = SINGLE;  // No duplicate quads (Sord is a set)
	}

	ZixBTree* const db  = model->indices[index_order];
	ZixBTreeIter*   cur = NULL;
	zix_btree_lower_bound(db, pat, &cur);
	if (zix_btree_iter_is_end(cur)) {
		SORD_FIND_LOG("No match found\n");
		zix_btree_iter_free(cur);
		return NULL;
	}
	const SordNode** const key = (const SordNode**)zix_btree_get(cur);
	if (!key || ( (mode == RANGE || mode == SINGLE)
	              && !sord_quad_match_inline(pat, key) )) {
		SORD_FIND_LOG("No match found\n");
		zix_btree_iter_free(cur);
		return NULL;
	}

	return sord_iter_new(model, cur, pat, index_order, mode, n_prefix);
}

SordIter*
sord_search(SordModel*      model,
            const SordNode* s,
            const SordNode* p,
            const SordNode* o,
            const SordNode* g)
{
	SordQuad pat = { s, p, o, g };
	return sord_find(model, pat);
}

SordNode*
sord_get(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g)
{
	if ((bool)s + (bool)p + (bool)o != 2) {
		return NULL;
	}

	SordIter* i   = sord_search(model, s, p, o, g);
	SordNode* ret = NULL;
	if (!s) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_SUBJECT));
	} else if (!p) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_PREDICATE));
	} else if (!o) {
		ret = sord_node_copy(sord_iter_get_node(i, SORD_OBJECT));
	}

	sord_iter_free(i);
	return ret;
}

bool
sord_ask(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g)
{
	SordQuad pat = { s, p, o, g };
	return sord_contains(model, pat);
}

uint64_t
sord_count(SordModel*      model,
           const SordNode* s,
           const SordNode* p,
           const SordNode* o,
           const SordNode* g)
{
	SordIter* i = sord_search(model, s, p, o, g);
	uint64_t  n = 0;
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		++n;
	}
	sord_iter_free(i);
	return n;
}

bool
sord_contains(SordModel* model, const SordQuad pat)
{
	SordIter* iter = sord_find(model, pat);
	bool      ret  = (iter != NULL);
	sord_iter_free(iter);
	return ret;
}

static uint8_t*
sord_strndup(const uint8_t* str, size_t len)
{
	uint8_t* dup = (uint8_t*)malloc(len + 1);
	memcpy(dup, str, len + 1);
	return dup;
}

SordNodeType
sord_node_get_type(const SordNode* node)
{
	switch (node->node.type) {
	case SERD_URI:
		return SORD_URI;
	case SERD_BLANK:
		return SORD_BLANK;
	default:
		return SORD_LITERAL;
	}
	SORD_UNREACHABLE();
}

const uint8_t*
sord_node_get_string(const SordNode* node)
{
	return node->node.buf;
}

const uint8_t*
sord_node_get_string_counted(const SordNode* node, size_t* bytes)
{
	*bytes = node->node.n_bytes;
	return node->node.buf;
}

const uint8_t*
sord_node_get_string_measured(const SordNode* node,
                              size_t*         bytes,
                              size_t*         chars)
{
	*bytes = node->node.n_bytes;
	*chars = node->node.n_chars;
	return node->node.buf;
}

const char*
sord_node_get_language(const SordNode* node)
{
	if (node->node.type != SERD_LITERAL || !node->meta.lit.lang[0]) {
		return NULL;
	}
	return node->meta.lit.lang;
}

SordNode*
sord_node_get_datatype(const SordNode* node)
{
	return (node->node.type == SERD_LITERAL) ? node->meta.lit.datatype : NULL;
}

SerdNodeFlags
sord_node_get_flags(const SordNode* node)
{
	return node->node.flags;
}

bool
sord_node_is_inline_object(const SordNode* node)
{
	return (node->node.type == SERD_BLANK) && (node->meta.res.refs_as_obj == 1);
}

static SordNode*
sord_insert_node(SordWorld* world, const SordNode* key, bool copy)
{
	SordNode* node = NULL;
	ZixStatus st   = zix_hash_insert(world->nodes, key, (const void**)&node);
	switch (st) {
	case ZIX_STATUS_EXISTS:
		++node->refs;
		break;
	case ZIX_STATUS_SUCCESS:
		assert(node->refs == 1);
		if (copy) {
			node->node.buf = sord_strndup(node->node.buf, node->node.n_bytes);
		}
		if (node->node.type == SERD_LITERAL) {
			node->meta.lit.datatype = sord_node_copy(node->meta.lit.datatype);
		}
		return node;
	default:
		error(world, SERD_ERR_INTERNAL,
		      "error inserting node `%s'\n", key->node.buf);
	}

	if (!copy) {
		// Free the buffer we would have copied if a new node was created
		free((uint8_t*)key->node.buf);
	}

	return node;
}

static SordNode*
sord_new_uri_counted(SordWorld* world, const uint8_t* str,
                     size_t n_bytes, size_t n_chars, bool copy)
{
	if (!serd_uri_string_has_scheme(str)) {
		error(world, SERD_ERR_BAD_ARG,
		      "attempt to map invalid URI `%s'\n", str);
		return NULL;  // Can't intern relative URIs
	}

	const SordNode key = {
		{ str, n_bytes, n_chars, 0, SERD_URI }, 1, { { 0 } }
	};

	return sord_insert_node(world, &key, copy);
}

SordNode*
sord_new_uri(SordWorld* world, const uint8_t* uri)
{
	const SerdNode node = serd_node_from_string(SERD_URI, uri);
	return sord_new_uri_counted(world, uri, node.n_bytes, node.n_chars, true);
}

SordNode*
sord_new_relative_uri(SordWorld*     world,
                      const uint8_t* uri,
                      const uint8_t* base_uri)
{
	if (serd_uri_string_has_scheme(uri)) {
		return sord_new_uri(world, uri);
	}
	SerdURI  buri = SERD_URI_NULL;
	SerdNode base = serd_node_new_uri_from_string(base_uri, NULL, &buri);
	SerdNode node = serd_node_new_uri_from_string(uri, &buri, NULL);

	SordNode* ret = sord_new_uri_counted(
		world, node.buf, node.n_bytes, node.n_chars, false);

	serd_node_free(&base);
	return ret;
}

static SordNode*
sord_new_blank_counted(SordWorld* world, const uint8_t* str,
                       size_t n_bytes, size_t n_chars)
{
	const SordNode key = {
		{ str, n_bytes, n_chars, 0, SERD_BLANK }, 1, { { 0 } }
	};

	return sord_insert_node(world, &key, true);
}

SordNode*
sord_new_blank(SordWorld* world, const uint8_t* str)
{
	const SerdNode node = serd_node_from_string(SERD_URI, str);
	return sord_new_blank_counted(world, str, node.n_bytes, node.n_chars);
}

static SordNode*
sord_new_literal_counted(SordWorld*     world,
                         SordNode*      datatype,
                         const uint8_t* str,
                         size_t         n_bytes,
                         size_t         n_chars,
                         SerdNodeFlags  flags,
                         const char*    lang)
{
	SordNode key = {
		{ str, n_bytes, n_chars, flags, SERD_LITERAL }, 1, { { 0 } }
	};
	key.meta.lit.datatype = sord_node_copy(datatype);
	memset(key.meta.lit.lang, 0, sizeof(key.meta.lit.lang));
	if (lang) {
		strncpy(key.meta.lit.lang, lang, sizeof(key.meta.lit.lang));
	}

	return sord_insert_node(world, &key, true);
}

SordNode*
sord_new_literal(SordWorld* world, SordNode* datatype,
                 const uint8_t* str, const char* lang)
{
	SerdNodeFlags flags   = 0;
	size_t        n_bytes = 0;
	size_t        n_chars = serd_strlen(str, &n_bytes, &flags);
	return sord_new_literal_counted(world, datatype,
	                                str, n_bytes, n_chars, flags,
	                                lang);
}

SordNode*
sord_node_from_serd_node(SordWorld*      world,
                         SerdEnv*        env,
                         const SerdNode* node,
                         const SerdNode* datatype,
                         const SerdNode* lang)
{
	if (!node) {
		return NULL;
	}

	SordNode* datatype_node = NULL;
	SordNode* ret           = NULL;
	switch (node->type) {
	case SERD_NOTHING:
		return NULL;
	case SERD_LITERAL:
		datatype_node = sord_node_from_serd_node(
			world, env, datatype, NULL, NULL),
		ret = sord_new_literal_counted(
			world,
			datatype_node,
			node->buf,
			node->n_bytes,
			node->n_chars,
			node->flags,
			lang ? (const char*)lang->buf : NULL);
		sord_node_free(world, datatype_node);
		return ret;
	case SERD_URI:
		if (serd_uri_string_has_scheme(node->buf)) {
			return sord_new_uri_counted(
				world, node->buf, node->n_bytes, node->n_chars, true);
		} else {
			SerdURI base_uri;
			serd_env_get_base_uri(env, &base_uri);
			SerdURI  abs_uri;
			SerdNode abs_uri_node = serd_node_new_uri_from_node(
				node, &base_uri, &abs_uri);
			ret = sord_new_uri_counted(world,
			                           abs_uri_node.buf,
			                           abs_uri_node.n_bytes,
			                           abs_uri_node.n_chars,
			                           true);
			serd_node_free(&abs_uri_node);
			return ret;
		}
	case SERD_CURIE: {
		SerdChunk uri_prefix;
		SerdChunk uri_suffix;
		if (serd_env_expand(env, node, &uri_prefix, &uri_suffix)) {
			error(world, SERD_ERR_BAD_CURIE,
			      "failed to expand CURIE `%s'\n", node->buf);
			return NULL;
		}
		const size_t uri_len = uri_prefix.len + uri_suffix.len;
		uint8_t*     buf     = (uint8_t*)malloc(uri_len + 1);
		memcpy(buf,                  uri_prefix.buf, uri_prefix.len);
		memcpy(buf + uri_prefix.len, uri_suffix.buf, uri_suffix.len);
		buf[uri_len] = '\0';
		ret = sord_new_uri_counted(
			world, buf, uri_len, serd_strlen(buf, NULL, NULL), false);
		return ret;
	}
	case SERD_BLANK:
		return sord_new_blank_counted(
			world, node->buf, node->n_bytes, node->n_chars);
	}
	return NULL;
}

const SerdNode*
sord_node_to_serd_node(const SordNode* node)
{
	return node ? &node->node : &SERD_NODE_NULL;
}

void
sord_node_free(SordWorld* world, SordNode* node)
{
	if (!node) {
		return;
	} else if (node->refs == 0) {
		error(world, SERD_ERR_BAD_ARG, "attempt to free garbage node\n");
	} else if (--node->refs == 0) {
		sord_node_free_internal(world, node);
	}
}

SordNode*
sord_node_copy(const SordNode* node)
{
	SordNode* copy = (SordNode*)node;
	if (copy) {
		++copy->refs;
	}
	return copy;
}

static inline bool
sord_add_to_index(SordModel* model, const SordNode** tup, SordOrder order)
{
	return !zix_btree_insert(model->indices[order], tup);
}

bool
sord_add(SordModel* model, const SordQuad tup)
{
	SORD_WRITE_LOG("Add " TUP_FMT "\n", TUP_FMT_ARGS(tup));
	if (!tup[0] || !tup[1] || !tup[2]) {
		error(model->world, SERD_ERR_BAD_ARG,
		      "attempt to add quad with NULL field\n");
		return false;
	} else if (model->n_iters > 0) {
		error(model->world, SERD_ERR_BAD_ARG, "added tuple during iteration\n");
	}

	const SordNode** quad = (const SordNode**)malloc(sizeof(SordQuad));
	memcpy(quad, tup, sizeof(SordQuad));

	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (!sord_add_to_index(model, quad, (SordOrder)i)) {
				assert(i == 0);  // Assuming index coherency
				free(quad);
				return false;  // Quad already stored, do nothing
			}
		}
	}

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_add_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	++model->n_quads;
	return true;
}

void
sord_remove(SordModel* model, const SordQuad tup)
{
	SORD_WRITE_LOG("Remove " TUP_FMT "\n", TUP_FMT_ARGS(tup));
	if (model->n_iters > 0) {
		error(model->world, SERD_ERR_BAD_ARG, "remove with iterator\n");
	}

	SordNode* quad = NULL;
	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (zix_btree_remove(model->indices[i], tup, (void**)&quad, NULL)) {
				assert(i == 0);  // Assuming index coherency
				return;  // Quad not found, do nothing
			}
		}
	}

	free(quad);

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_drop_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	--model->n_quads;
}

SerdStatus
sord_erase(SordModel* model, SordIter* iter)
{
	if (model->n_iters > 1) {
		error(model->world, SERD_ERR_BAD_ARG, "erased with many iterators\n");
		return SERD_ERR_BAD_ARG;
	}

	SordQuad tup;
	sord_iter_get(iter, tup);

	SORD_WRITE_LOG("Remove " TUP_FMT "\n", TUP_FMT_ARGS(tup));

	SordNode* quad = NULL;
	for (unsigned i = 0; i < NUM_ORDERS; ++i) {
		if (model->indices[i] && (i < GSPO || tup[3])) {
			if (zix_btree_remove(model->indices[i], tup, (void**)&quad,
			                     i == iter->order ? &iter->cur : NULL)) {
				return (i == 0) ? SERD_ERR_NOT_FOUND : SERD_ERR_INTERNAL;
			}
		}
	}
	iter->end = zix_btree_iter_is_end(iter->cur);
	sord_iter_scan_next(iter);

	free(quad);

	for (int i = 0; i < TUP_LEN; ++i) {
		sord_drop_quad_ref(model, tup[i], (SordQuadIndex)i);
	}

	--model->n_quads;
	return SERD_SUCCESS;
}
struct SordInserterImpl {
	SordModel* model;
	SerdEnv* env;
};

SordInserter*
sord_inserter_new(SordModel* model,
	SerdEnv* env)
{
	SordInserter* inserter = (SordInserter*)malloc(sizeof(SordInserter));
	inserter->model = model;
	inserter->env = env;
	return inserter;
}

void
sord_inserter_free(SordInserter* inserter)
{
	free(inserter);
}

SerdStatus
sord_inserter_set_base_uri(SordInserter* inserter,
	const SerdNode* uri)
{
	return serd_env_set_base_uri(inserter->env, uri);
}

SerdStatus
sord_inserter_set_prefix(SordInserter* inserter,
	const SerdNode* name,
	const SerdNode* uri)
{
	return serd_env_set_prefix(inserter->env, name, uri);
}

SerdStatus
sord_inserter_write_statement(SordInserter* inserter,
	SerdStatementFlags flags,
	const SerdNode* graph,
	const SerdNode* subject,
	const SerdNode* predicate,
	const SerdNode* object,
	const SerdNode* object_datatype,
	const SerdNode* object_lang)
{
	SordWorld* world = sord_get_world(inserter->model);
	SerdEnv* env = inserter->env;

	SordNode* g = sord_node_from_serd_node(world, env, graph, NULL, NULL);
	SordNode* s = sord_node_from_serd_node(world, env, subject, NULL, NULL);
	SordNode* p = sord_node_from_serd_node(world, env, predicate, NULL, NULL);
	SordNode* o = sord_node_from_serd_node(world, env, object,
		object_datatype, object_lang);

	if (!s || !p || !o) {
		return SERD_ERR_BAD_ARG;
	}

	const SordQuad tup = { s, p, o, g };
	sord_add(inserter->model, tup);

	sord_node_free(world, o);
	sord_node_free(world, p);
	sord_node_free(world, s);
	sord_node_free(world, g);

	return SERD_SUCCESS;
}

SORD_API
SerdReader*
sord_new_reader(SordModel* model,
	SerdEnv* env,
	SerdSyntax syntax,
	SordNode* graph)
{
	SordInserter* inserter = sord_inserter_new(model, env);

	SerdReader* reader = serd_reader_new(
		syntax, inserter, (void (*)(void* ptr))sord_inserter_free,
		(SerdBaseSink)sord_inserter_set_base_uri,
		(SerdPrefixSink)sord_inserter_set_prefix,
		(SerdStatementSink)sord_inserter_write_statement,
		NULL);

	if (graph) {
		serd_reader_set_default_graph(reader, sord_node_to_serd_node(graph));
	}

	return reader;
}

static SerdStatus
write_statement(SordModel* sord,
	SerdWriter* writer,
	SordQuad           tup,
	SerdStatementFlags flags)
{
	const SordNode* s = tup[SORD_SUBJECT];
	const SordNode* p = tup[SORD_PREDICATE];
	const SordNode* o = tup[SORD_OBJECT];
	const SordNode* d = sord_node_get_datatype(o);
	const SerdNode* ss = sord_node_to_serd_node(s);
	const SerdNode* sp = sord_node_to_serd_node(p);
	const SerdNode* so = sord_node_to_serd_node(o);
	const SerdNode* sd = sord_node_to_serd_node(d);

	const char* lang_str = sord_node_get_language(o);
	size_t      lang_len = lang_str ? strlen(lang_str) : 0;
	SerdNode    language = SERD_NODE_NULL;
	if (lang_str) {
		language.type = SERD_LITERAL;
		language.n_bytes = lang_len;
		language.n_chars = lang_len;
		language.buf = (const uint8_t*)lang_str;
	};

	// TODO: Subject abbreviation

	if (sord_node_is_inline_object(s) && !(flags & SERD_ANON_CONT)) {
		return SERD_SUCCESS;
	}

	SerdStatus st = SERD_SUCCESS;
	if (sord_node_is_inline_object(o)) {
		SordQuad  sub_pat = { o, 0, 0, 0 };
		SordIter* sub_iter = sord_find(sord, sub_pat);

		SerdStatementFlags start_flags = flags
			| ((sub_iter) ? SERD_ANON_O_BEGIN : SERD_EMPTY_O);

		st = serd_writer_write_statement(
			writer, start_flags, NULL, ss, sp, so, sd, &language);

		if (!st && sub_iter) {
			flags |= SERD_ANON_CONT;
			for (; !st && !sord_iter_end(sub_iter); sord_iter_next(sub_iter)) {
				SordQuad sub_tup;
				sord_iter_get(sub_iter, sub_tup);
				st = write_statement(sord, writer, sub_tup, flags);
			}
			sord_iter_free(sub_iter);
			serd_writer_end_anon(writer, so);
		}
	}
	else {
		st = serd_writer_write_statement(
			writer, flags, NULL, ss, sp, so, sd, &language);
	}

	return st;
}

bool
sord_write(SordModel* model,
	SerdWriter* writer,
	SordNode* graph)
{
	SordQuad  pat = { 0, 0, 0, graph };
	SordIter* iter = sord_find(model, pat);
	return sord_write_iter(iter, writer);
}

bool
sord_write_iter(SordIter* iter,
	SerdWriter* writer)
{
	if (!iter) {
		return false;
	}

	SordModel* model = (SordModel*)sord_iter_get_model(iter);
	SerdStatus st = SERD_SUCCESS;
	for (; !st && !sord_iter_end(iter); sord_iter_next(iter)) {
		SordQuad tup;
		sord_iter_get(iter, tup);
		st = write_statement(model, writer, tup, 0);
	}
	sord_iter_free(iter);

	return !st;
}
