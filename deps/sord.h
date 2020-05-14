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

/**
   @file sord.h API for Sord, a lightweight RDF model library.
*/

#ifndef SORD_SORD_H
#define SORD_SORD_H
#define ZIX_API
#define ZIX_PRIVATE
#define SORD_API
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "serd/serd.h"


#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

typedef enum {
	ZIX_STATUS_SUCCESS,
	ZIX_STATUS_ERROR,
	ZIX_STATUS_NO_MEM,
	ZIX_STATUS_NOT_FOUND,
	ZIX_STATUS_EXISTS,
	ZIX_STATUS_BAD_ARG,
	ZIX_STATUS_BAD_PERMS,
} ZixStatus;

/**
   Function for comparing two elements.
*/
typedef int (*ZixComparator)(const void* a, const void* b, void* user_data);

/**
   Function for testing equality of two elements.
*/
typedef bool (*ZixEqualFunc)(const void* a, const void* b);

/**
   Function to destroy an element.
*/
typedef void (*ZixDestroyFunc)(void* ptr);

/**
   @}
*/

uint32_t zix_digest_start(void);

uint32_t zix_digest_add(uint32_t hash, const void* buf, size_t len);


typedef struct ZixHashImpl ZixHash;

/**
   Function for computing the hash of an element.
*/
typedef uint32_t(*ZixHashFunc)(const void* value);

/**
   Function to visit a hash element.
*/
typedef void (*ZixHashVisitFunc)(void* value,
	void* user_data);

/**
   Create a new hash table.

   To minimize space overhead, unlike many hash tables this stores a single
   value, not a key and a value.  Any size of value can be stored, but all the
   values in the hash table must be the same size, and the values must be safe
   to copy with memcpy.  To get key:value behaviour, simply insert a struct
   with a key and value into the hash.

   @param hash_func The hashing function.
   @param equal_func A function to test value equality.
   @param value_size The size of the values to be stored.
*/
ZIX_API ZixHash*
zix_hash_new(ZixHashFunc  hash_func,
	ZixEqualFunc equal_func,
	size_t       value_size);

/**
   Free `hash`.
*/
ZIX_API void
zix_hash_free(ZixHash* hash);

/**
   Return the number of elements in `hash`.
*/
ZIX_API size_t
zix_hash_size(const ZixHash* hash);

/**
   Insert an item into `hash`.

   If no matching value is found, ZIX_STATUS_SUCCESS will be returned, and @p
   inserted will be pointed to the copy of `value` made in the new hash node.

   If a matching value already exists, ZIX_STATUS_EXISTS will be returned, and
   `inserted` will be pointed to the existing value.

   @param hash The hash table.
   @param value The value to be inserted.
   @param inserted The copy of `value` in the hash table.
   @return ZIX_STATUS_SUCCESS, ZIX_STATUS_EXISTS, or ZIX_STATUS_NO_MEM.
*/
ZIX_API ZixStatus
zix_hash_insert(ZixHash* hash,
	const void* value,
	const void** inserted);

/**
   Remove an item from `hash`.

   @param hash The hash table.
   @param value The value to remove.
   @return ZIX_STATUS_SUCCES or ZIX_STATUS_NOT_FOUND.
*/
ZIX_API ZixStatus
zix_hash_remove(ZixHash* hash,
	const void* value);

/**
   Search for an item in `hash`.

   @param hash The hash table.
   @param value The value to search for.
*/
ZIX_API const void*
zix_hash_find(const ZixHash* hash,
	const void* value);

/**
   Call `f` on each value in `hash`.

   @param hash The hash table.
   @param f The function to call on each value.
   @param user_data The user_data parameter passed to `f`.
*/
ZIX_API void
zix_hash_foreach(ZixHash* hash,
	ZixHashVisitFunc f,
	void* user_data);




/**
   A B-Tree.
*/
typedef struct ZixBTreeImpl ZixBTree;

/**
   A B-Tree node (opaque).
*/
typedef struct ZixBTreeNodeImpl ZixBTreeNode;

/**
   An iterator over a B-Tree.

   Note that modifying the trees invalidates all iterators, so all iterators
   are const iterators.
*/
typedef struct ZixBTreeIterImpl ZixBTreeIter;

/**
   Create a new (empty) B-Tree.
*/
ZIX_API ZixBTree*
zix_btree_new(ZixComparator  cmp,
	void* cmp_data,
	ZixDestroyFunc destroy);

/**
   Free `t`.
*/
ZIX_API void
zix_btree_free(ZixBTree* t);

/**
   Return the number of elements in `t`.
*/
ZIX_API size_t
zix_btree_size(const ZixBTree* t);

/**
   Insert the element `e` into `t`.
*/
ZIX_API ZixStatus
zix_btree_insert(ZixBTree* t, void* e);

/**
   Remove the value `e` from `t`.

   @param t Tree to remove from.

   @param e Value to remove.

   @param out Set to point to the removed pointer (which may not equal `e`).

   @param next If non-NULL, pointed to the value following `e`.  If *next is
   also non-NULL, the iterator is reused, otherwise a new one is allocated.  To
   reuse an iterator, no items may have been added since its creation.
*/
ZIX_API ZixStatus
zix_btree_remove(ZixBTree* t, const void* e, void** out, ZixBTreeIter** next);

/**
   Set `ti` to an element equal to `e` in `t`.
   If no such item exists, `ti` is set to NULL.
*/
ZIX_API ZixStatus
zix_btree_find(const ZixBTree* t, const void* e, ZixBTreeIter** ti);

/**
   Set `ti` to the smallest element in `t` that is not less than `e`.

   Wildcards are supported, so if the search key `e` compares equal to many
   values in the tree, `ti` will be set to the least such element.  The search
   key `e` is always passed as the second argument to the comparator.
*/
ZIX_API ZixStatus
zix_btree_lower_bound(const ZixBTree* t, const void* e, ZixBTreeIter** ti);

/**
   Return the data associated with the given tree item.
*/
ZIX_API void*
zix_btree_get(const ZixBTreeIter* ti);

/**
   Return an iterator to the first (smallest) element in `t`.

   The returned iterator must be freed with zix_btree_iter_free().
*/
ZIX_API ZixBTreeIter*
zix_btree_begin(const ZixBTree* t);

/**
   Return true iff `i` is an iterator to the end of its tree.
*/
ZIX_API bool
zix_btree_iter_is_end(const ZixBTreeIter* i);

/**
   Increment `i` to point to the next element in the tree.
*/
ZIX_API void
zix_btree_iter_increment(ZixBTreeIter* i);

/**
   Free `i`.
*/
ZIX_API void
zix_btree_iter_free(ZixBTreeIter* i);




/**
   @defgroup sord Sord
   A lightweight RDF model library.

   Sord stores RDF (subject object predicate context) quads, where the context
   may be omitted (to represent triples in the default graph).
   @{
*/

/**
   Sord World.
   The World represents all library state, including interned strings.
*/
typedef struct SordWorldImpl SordWorld;

/**
   Sord Model.

   A model is an indexed set of Quads (i.e. it can contain several RDF
   graphs).  It may be searched using various patterns depending on which
   indices are enabled.
*/
typedef struct SordModelImpl SordModel;

/**
   Model Inserter.

   An inserter is used for writing statements to a model using the Serd sink
   interface.  This makes it simple to write to a model directly using a
   SerdReader, or any other code that writes statements to a SerdStatementSink.
*/
typedef struct SordInserterImpl SordInserter;

/**
   Model Iterator.
*/
typedef struct SordIterImpl SordIter;

/**
   RDF Node.
   A Node is a component of a Quad.  Nodes may be URIs, blank nodes, or
   (in the case of quad objects only) string literals. Literal nodes may
   have an associate language or datatype (but not both).
*/
typedef struct SordNodeImpl SordNode;

/**
   Quad of nodes (a statement), or a quad pattern.

   Nodes are ordered (S P O G).  The ID of the default graph is 0.
*/
typedef const SordNode* SordQuad[4];

/**
   Index into a SordQuad.
*/
typedef enum {
	SORD_SUBJECT = 0,  /**< Subject */
	SORD_PREDICATE = 1,  /**< Predicate ("key") */
	SORD_OBJECT = 2,  /**< Object    ("value") */
	SORD_GRAPH = 3   /**< Graph     ("context") */
} SordQuadIndex;

/**
   Type of a node.
*/
typedef enum {
	SORD_URI = 1,  /**< URI */
	SORD_BLANK = 2,  /**< Blank node identifier */
	SORD_LITERAL = 3   /**< Literal (string with optional lang or datatype) */
} SordNodeType;

/**
   Indexing option.
*/
typedef enum {
	SORD_SPO = 1,       /**< Subject,   Predicate, Object */
	SORD_SOP = 1 << 1,  /**< Subject,   Object,    Predicate */
	SORD_OPS = 1 << 2,  /**< Object,    Predicate, Subject */
	SORD_OSP = 1 << 3,  /**< Object,    Subject,   Predicate */
	SORD_PSO = 1 << 4,  /**< Predicate, Subject,   Object */
	SORD_POS = 1 << 5   /**< Predicate, Object,    Subject */
} SordIndexOption;

/**
   @name World
   @{
*/

/**
   Create a new Sord World.
   It is safe to use multiple worlds in one process, though no data
   (e.g. nodes) can be shared between worlds, and this should be avoided if
   possible for performance reasons.
*/
SORD_API
SordWorld*
sord_world_new(void);

/**
   Free `world`.
*/
SORD_API
void
sord_world_free(SordWorld* world);

/**
   Set a function to be called when errors occur.

   The `error_sink` will be called with `handle` as its first argument.  If
   no error function is set, errors are printed to stderr.
*/
SORD_API
void
sord_world_set_error_sink(SordWorld* world,
	SerdErrorSink error_sink,
	void* handle);



/**
   Get a URI node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_uri(SordWorld* world, const uint8_t* uri);

/**
   Get a URI node from a relative URI string.
*/
SORD_API
SordNode*
sord_new_relative_uri(SordWorld* world,
	const uint8_t* uri,
	const uint8_t* base_uri);

/**
   Get a blank node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_blank(SordWorld* world, const uint8_t* str);

/**
   Get a literal node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_literal(SordWorld* world,
	SordNode* datatype,
	const uint8_t* str,
	const char* lang);

/**
   Copy a node (obtain a reference).

   Node that since nodes are interned and reference counted, this does not
   actually create a deep copy of `node`.
*/
SORD_API
SordNode*
sord_node_copy(const SordNode* node);

/**
   Free a node (drop a reference).
*/
SORD_API
void
sord_node_free(SordWorld* world, SordNode* node);

/**
   Return the type of a node (SORD_URI, SORD_BLANK, or SORD_LITERAL).
*/
SORD_API
SordNodeType
sord_node_get_type(const SordNode* node);

/**
   Return the string value of a node.
*/
SORD_API
const uint8_t*
sord_node_get_string(const SordNode* node);

/**
   Return the string value of a node, and set `bytes` to its length in bytes.
*/
SORD_API
const uint8_t*
sord_node_get_string_counted(const SordNode* node, size_t* bytes);

/**
   Return the string value of a node, and set `bytes` to its length in bytes,
   and `count` to its length in characters.
*/
SORD_API
const uint8_t*
sord_node_get_string_measured(const SordNode* node,
	size_t* bytes,
	size_t* chars);

/**
   Return the language of a literal node (or NULL).
*/
SORD_API
const char*
sord_node_get_language(const SordNode* node);

/**
   Return the datatype URI of a literal node (or NULL).
*/
SORD_API
SordNode*
sord_node_get_datatype(const SordNode* node);

/**
   Return the flags (string attributes) of a node.
*/
SORD_API
SerdNodeFlags
sord_node_get_flags(const SordNode* node);

/**
   Return true iff node can be serialised as an inline object.

   More specifically, this returns true iff the node is the object field
   of exactly one statement, and therefore can be inlined since it needn't
   be referred to by name.
*/
SORD_API
bool
sord_node_is_inline_object(const SordNode* node);

/**
   Return true iff `a` is equal to `b`.

   Note this is much faster than comparing the node's strings.
*/
SORD_API
bool
sord_node_equals(const SordNode* a,
	const SordNode* b);

/**
   Return a SordNode as a SerdNode.

   The returned node is shared and must not be freed or modified.
*/
SORD_API
const SerdNode*
sord_node_to_serd_node(const SordNode* node);

/**
   Create a new SordNode from a SerdNode.

   The returned node must be freed using sord_node_free().
*/
SORD_API
SordNode*
sord_node_from_serd_node(SordWorld* world,
	SerdEnv* env,
	const SerdNode* node,
	const SerdNode* datatype,
	const SerdNode* lang);

/**
   Create a new model.

   @param world The world in which to make this model.

   @param indices SordIndexOption flags (e.g. SORD_SPO|SORD_OPS).  Be sure to
   enable an index where the most significant node(s) are not variables in your
   queries (e.g. to make (? P O) queries, enable either SORD_OPS or SORD_POS).

   @param graphs If true, store (and index) graph contexts.
*/
SORD_API
SordModel*
sord_new(SordWorld* world,
	unsigned  indices,
	bool      graphs);

/**
   Close and free `model`.
*/
SORD_API
void
sord_free(SordModel* model);

/**
   Get the world associated with `model`.
*/
SORD_API
SordWorld*
sord_get_world(SordModel* model);

/**
   Return the number of nodes stored in `world`.

   Nodes are included in this count iff they are a part of a quad in `world`.
*/
SORD_API
size_t
sord_num_nodes(const SordWorld* world);

/**
   Return the number of quads stored in `model`.
*/
SORD_API
size_t
sord_num_quads(const SordModel* model);

/**
   Return an iterator to the start of `model`.
*/
SORD_API
SordIter*
sord_begin(const SordModel* model);

/**
   Search for statements by a quad pattern.
   @return an iterator to the first match, or NULL if no matches found.
*/
SORD_API
SordIter*
sord_find(SordModel* model, const SordQuad pat);

/**
   Search for statements by nodes.
   @return an iterator to the first match, or NULL if no matches found.
*/
SORD_API
SordIter*
sord_search(SordModel* model,
	const SordNode* s,
	const SordNode* p,
	const SordNode* o,
	const SordNode* g);
/**
   Search for a single node that matches a pattern.
   Exactly one of `s`, `p`, `o` must be NULL.
   This function is mainly useful for predicates that only have one value.
   The returned node must be freed using sord_node_free().
   @return the first matching node, or NULL if no matches are found.
*/
SORD_API
SordNode*
sord_get(SordModel* model,
	const SordNode* s,
	const SordNode* p,
	const SordNode* o,
	const SordNode* g);

/**
   Return true iff a statement exists.
*/
SORD_API
bool
sord_ask(SordModel* model,
	const SordNode* s,
	const SordNode* p,
	const SordNode* o,
	const SordNode* g);

/**
   Return the number of matching statements.
*/
SORD_API
uint64_t
sord_count(SordModel* model,
	const SordNode* s,
	const SordNode* p,
	const SordNode* o,
	const SordNode* g);

/**
   Check if `model` contains a triple pattern.

   @return true if `model` contains a match for `pat`, otherwise false.
*/
SORD_API
bool
sord_contains(SordModel* model, const SordQuad pat);

/**
   Add a quad to a model.

   Calling this function invalidates all iterators on `model`.

   @return true on success, false, on error.
*/
SORD_API
bool
sord_add(SordModel* model, const SordQuad tup);

/**
   Remove a quad from a model.

   Calling this function invalidates all iterators on `model`.  To remove quads
   while iterating, use sord_erase() instead.
*/
SORD_API
void
sord_remove(SordModel* model, const SordQuad tup);

/**
   Remove a quad from a model via an iterator.

   Calling this function invalidates all iterators on `model` except `iter`.

   @param model The model which `iter` points to.
   @param iter Iterator to the element to erase, which is incremented to the
   next value on return.
*/
SORD_API
SerdStatus
sord_erase(SordModel* model, SordIter* iter);

/**
   @}
   @name Inserter
   @{
*/

/**
   Create an inserter for writing statements to a model.
*/
SORD_API
SordInserter*
sord_inserter_new(SordModel* model,
	SerdEnv* env);

/**
   Free an inserter.
*/
SORD_API
void
sord_inserter_free(SordInserter* inserter);

/**
   Set the current base URI for writing to the model.

   Note this function can be safely casted to SerdBaseSink.
*/
SORD_API
SerdStatus
sord_inserter_set_base_uri(SordInserter* inserter,
	const SerdNode* uri);

/**
   Set a namespace prefix for writing to the model.

   Note this function can be safely casted to SerdPrefixSink.
*/
SORD_API
SerdStatus
sord_inserter_set_prefix(SordInserter* inserter,
	const SerdNode* name,
	const SerdNode* uri);

/**
   Write a statement to the model.

   Note this function can be safely casted to SerdStatementSink.
*/
SORD_API
SerdStatus
sord_inserter_write_statement(SordInserter* inserter,
	SerdStatementFlags flags,
	const SerdNode* graph,
	const SerdNode* subject,
	const SerdNode* predicate,
	const SerdNode* object,
	const SerdNode* object_datatype,
	const SerdNode* object_lang);

/**
   @}
   @name Iteration
   @{
*/

/**
   Set `quad` to the quad pointed to by `iter`.
*/
SORD_API
void
sord_iter_get(const SordIter* iter, SordQuad tup);

/**
   Return a field of the quad pointed to by `iter`.

   Returns NULL if `iter` is NULL or is at the end.
*/
SORD_API
const SordNode*
sord_iter_get_node(const SordIter* iter, SordQuadIndex index);

/**
   Return the store pointed to by `iter`.
*/
SORD_API
const SordModel*
sord_iter_get_model(SordIter* iter);

/**
   Increment `iter` to point to the next statement.
*/
SORD_API
bool
sord_iter_next(SordIter* iter);

/**
   Return true iff `iter` is at the end of its range.
*/
SORD_API
bool
sord_iter_end(const SordIter* iter);

/**
   Free `iter`.
*/
SORD_API
void
sord_iter_free(SordIter* iter);

/**
   @}
   @name Utilities
   @{
*/

/**
   Match two quads (using ID comparison only).

   This function is a straightforward and fast equivalence match with wildcard
   support (ID 0 is a wildcard). It does not actually read node data.
   @return true iff `x` and `y` match.
*/
SORD_API
bool
sord_quad_match(const SordQuad x, const SordQuad y);

/**
   @}
   @name Serialisation
   @{
*/

/**
   Return a reader that will read into `model`.
*/
SORD_API
SerdReader*
sord_new_reader(SordModel* model,
	SerdEnv* env,
	SerdSyntax syntax,
	SordNode* graph);

/**
   Write a model to a writer.
*/
SORD_API
bool
sord_write(SordModel* model,
	SerdWriter* writer,
	SordNode* graph);

/**
   Write a range to a writer.

   This increments `iter` to its end, then frees it.
*/
SORD_API
bool
sord_write_iter(SordIter* iter,
	SerdWriter* writer);

/**
   @}
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* SORD_SORD_H */
