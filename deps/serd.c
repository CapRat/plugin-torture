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

#include "serd.h"
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <float.h>


#define _POSIX_C_SOURCE 200809L /* for posix_memalign and posix_fadvise */



//#include "serd_config.h"

#if defined(HAVE_POSIX_FADVISE) && defined(HAVE_FILENO)
#   include <fcntl.h>
#endif

#define NS_XSD "http://www.w3.org/2001/XMLSchema#"
#define NS_RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#define SERD_PAGE_SIZE 4096

#ifndef MIN
#    define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif

#if defined(__GNUC__)
#    define SERD_LOG_FUNC(fmt, arg1) __attribute__((format(printf, fmt, arg1)))
#else
#    define SERD_LOG_FUNC(fmt, arg1)
#endif

static const uint8_t replacement_char[] = { 0xEF, 0xBF, 0xBD };

/* File and Buffer Utilities */

static inline FILE*
serd_fopen(const char* path, const char* mode)
{
	FILE* fd = fopen(path, mode);
	if (!fd) {
		fprintf(stderr, "error: failed to open file %s (%s)\n",
			path, strerror(errno));
		return NULL;
	}
#if defined(HAVE_POSIX_FADVISE) && defined(HAVE_FILENO)
	posix_fadvise(fileno(fd), 0, 0, POSIX_FADV_SEQUENTIAL);
#endif
	return fd;
}

static inline void*
serd_bufalloc(size_t size)
{
#ifdef HAVE_POSIX_MEMALIGN
	void* ptr;
	const int ret = posix_memalign(&ptr, SERD_PAGE_SIZE, size);
	return ret ? NULL : ptr;
#else
	return malloc(size);
#endif
}

/* Byte source */

typedef struct {
	const uint8_t* filename;
	unsigned       line;
	unsigned       col;
} Cursor;

typedef struct {
	SerdSource          read_func;    ///< Read function (e.g. fread)
	SerdStreamErrorFunc error_func;   ///< Error function (e.g. ferror)
	void* stream;       ///< Stream (e.g. FILE)
	size_t              page_size;    ///< Number of bytes to read at a time
	size_t              buf_size;     ///< Number of bytes in file_buf
	Cursor              cur;          ///< Cursor for error reporting
	uint8_t* file_buf;     ///< Buffer iff reading pages from a file
	const uint8_t* read_buf;     ///< Pointer to file_buf or read_byte
	size_t              read_head;    ///< Offset into read_buf
	uint8_t             read_byte;    ///< 1-byte 'buffer' used when not paging
	bool                from_stream;  ///< True iff reading from `stream`
	bool                prepared;     ///< True iff prepared for reading
	bool                eof;          ///< True iff end of file reached
} SerdByteSource;

SerdStatus
serd_byte_source_open_file(SerdByteSource* source,
	FILE* file,
	bool            bulk);

SerdStatus
serd_byte_source_open_string(SerdByteSource* source, const uint8_t* utf8);

SerdStatus
serd_byte_source_open_source(SerdByteSource* source,
	SerdSource          read_func,
	SerdStreamErrorFunc error_func,
	void* stream,
	const uint8_t* name,
	size_t              page_size);

SerdStatus
serd_byte_source_close(SerdByteSource* source);

SerdStatus
serd_byte_source_prepare(SerdByteSource* source);

SerdStatus
serd_byte_source_page(SerdByteSource* source);

static inline uint8_t
serd_byte_source_peek(SerdByteSource* source)
{
	assert(source->prepared);
	return source->read_buf[source->read_head];
}

static inline SerdStatus
serd_byte_source_advance(SerdByteSource* source)
{
	SerdStatus st = SERD_SUCCESS;

	switch (serd_byte_source_peek(source)) {
	case '\n': ++source->cur.line; source->cur.col = 0; break;
	default:   ++source->cur.col;
	}

	const bool was_eof = source->eof;
	if (source->from_stream) {
		source->eof = false;
		if (source->page_size > 1) {
			if (++source->read_head == source->page_size) {
				st = serd_byte_source_page(source);
			}
			else if (source->read_head == source->buf_size) {
				source->eof = true;
			}
		}
		else {
			if (!source->read_func(&source->read_byte, 1, 1, source->stream)) {
				source->eof = true;
				st = source->error_func(source->stream) ? SERD_ERR_UNKNOWN
					: SERD_FAILURE;
			}
		}
	}
	else if (!source->eof) {
		++source->read_head; // Move to next character in string
		if (source->read_buf[source->read_head] == '\0') {
			source->eof = true;
		}
	}

	return (was_eof && source->eof) ? SERD_FAILURE : st;
}

/* Stack */

/** A dynamic stack in memory. */
typedef struct {
	uint8_t* buf;       ///< Stack memory
	size_t   buf_size;  ///< Allocated size of buf (>= size)
	size_t   size;      ///< Conceptual size of stack in buf
} SerdStack;

/** An offset to start the stack at. Note 0 is reserved for NULL. */
#define SERD_STACK_BOTTOM sizeof(void*)

static inline SerdStack
serd_stack_new(size_t size)
{
	SerdStack stack;
	stack.buf = (uint8_t*)calloc(size, 1);
	stack.buf_size = size;
	stack.size = SERD_STACK_BOTTOM;
	return stack;
}

static inline bool
serd_stack_is_empty(SerdStack* stack)
{
	return stack->size <= SERD_STACK_BOTTOM;
}

static inline void
serd_stack_free(SerdStack* stack)
{
	free(stack->buf);
	stack->buf = NULL;
	stack->buf_size = 0;
	stack->size = 0;
}

static inline uint8_t*
serd_stack_push(SerdStack* stack, size_t n_bytes)
{
	const size_t new_size = stack->size + n_bytes;
	if (stack->buf_size < new_size) {
		stack->buf_size += (stack->buf_size >> 1); // *= 1.5
		stack->buf = (uint8_t*)realloc(stack->buf, stack->buf_size);
	}
	uint8_t* const ret = (stack->buf + stack->size);
	stack->size = new_size;
	return ret;
}

static inline void
serd_stack_pop(SerdStack* stack, size_t n_bytes)
{
	assert(stack->size >= n_bytes);
	stack->size -= n_bytes;
}

static inline void*
serd_stack_push_aligned(SerdStack* stack, size_t n_bytes, size_t align)
{
	// Push one byte to ensure space for a pad count
	serd_stack_push(stack, 1);

	// Push padding if necessary
	const size_t pad = align - stack->size % align;
	if (pad > 0) {
		serd_stack_push(stack, pad);
	}

	// Set top of stack to pad count so we can properly pop later
	assert(pad < UINT8_MAX);
	stack->buf[stack->size - 1] = (uint8_t)pad;

	// Push requested space at aligned location
	return serd_stack_push(stack, n_bytes);
}

static inline void
serd_stack_pop_aligned(SerdStack* stack, size_t n_bytes)
{
	// Pop requested space down to aligned location
	serd_stack_pop(stack, n_bytes);

	// Get amount of padding from top of stack
	const uint8_t pad = stack->buf[stack->size - 1];

	// Pop padding and pad count
	serd_stack_pop(stack, pad + 1u);
}

/* Byte Sink */

typedef struct SerdByteSinkImpl {
	SerdSink sink;
	void* stream;
	uint8_t* buf;
	size_t   size;
	size_t   block_size;
} SerdByteSink;

static inline SerdByteSink
serd_byte_sink_new(SerdSink sink, void* stream, size_t block_size)
{
	SerdByteSink bsink;
	bsink.sink = sink;
	bsink.stream = stream;
	bsink.size = 0;
	bsink.block_size = block_size;
	bsink.buf = ((block_size > 1)
		? (uint8_t*)serd_bufalloc(block_size)
		: NULL);
	return bsink;
}

static inline void
serd_byte_sink_flush(SerdByteSink* bsink)
{
	if (bsink->block_size > 1 && bsink->size > 0) {
		bsink->sink(bsink->buf, bsink->size, bsink->stream);
		bsink->size = 0;
	}
}

static inline void
serd_byte_sink_free(SerdByteSink* bsink)
{
	serd_byte_sink_flush(bsink);
	free(bsink->buf);
	bsink->buf = NULL;
}

static inline size_t
serd_byte_sink_write(const void* buf, size_t len, SerdByteSink* bsink)
{
	if (len == 0) {
		return 0;
	}
	else if (bsink->block_size == 1) {
		return bsink->sink(buf, len, bsink->stream);
	}

	const size_t orig_len = len;
	while (len) {
		const size_t space = bsink->block_size - bsink->size;
		const size_t n = MIN(space, len);

		// Write as much as possible into the remaining buffer space
		memcpy(bsink->buf + bsink->size, buf, n);
		bsink->size += n;
		buf = (const uint8_t*)buf + n;
		len -= n;

		// Flush page if buffer is full
		if (bsink->size == bsink->block_size) {
			bsink->sink(bsink->buf, bsink->block_size, bsink->stream);
			bsink->size = 0;
		}
	}
	return orig_len;
}

/* Character utilities */

/** Return true if `c` lies within [`min`...`max`] (inclusive) */
static inline bool
in_range(const int c, const int min, const int max)
{
	return (c >= min && c <= max);
}

/** RFC2234: ALPHA ::= %x41-5A / %x61-7A  ; A-Z / a-z */
static inline bool
is_alpha(const int c)
{
	return in_range(c, 'A', 'Z') || in_range(c, 'a', 'z');
}

/** RFC2234: DIGIT ::= %x30-39  ; 0-9 */
static inline bool
is_digit(const int c)
{
	return in_range(c, '0', '9');
}

/* RFC2234: HEXDIG ::= DIGIT / "A" / "B" / "C" / "D" / "E" / "F" */
static inline bool
is_hexdig(const int c)
{
	return is_digit(c) || in_range(c, 'A', 'F');
}

/* Turtle / JSON / C: XDIGIT ::= DIGIT / A-F / a-f */
static inline bool
is_xdigit(const int c)
{
	return is_hexdig(c) || in_range(c, 'a', 'f');
}

static inline bool
is_space(const char c)
{
	switch (c) {
	case ' ': case '\f': case '\n': case '\r': case '\t': case '\v':
		return true;
	default:
		return false;
	}
}

static inline bool
is_base64(const uint8_t c)
{
	return is_alpha(c) || is_digit(c) || c == '+' || c == '/' || c == '=';
}

static inline bool
is_windows_path(const uint8_t* path)
{
	return is_alpha(path[0]) && (path[1] == ':' || path[1] == '|')
		&& (path[2] == '/' || path[2] == '\\');
}

/* String utilities */

size_t
serd_substrlen(const uint8_t* str,
	size_t         len,
	size_t* n_bytes,
	SerdNodeFlags* flags);

static inline int
serd_strncasecmp(const char* s1, const char* s2, size_t n)
{
	for (; n > 0 && *s2; s1++, s2++, --n) {
		if (toupper(*s1) != toupper(*s2)) {
			return ((*(const uint8_t*)s1 < *(const uint8_t*)s2) ? -1 : +1);
		}
	}
	return 0;
}

static inline uint32_t
utf8_num_bytes(const uint8_t c)
{
	if ((c & 0x80) == 0) {  // Starts with `0'
		return 1;
	}
	else if ((c & 0xE0) == 0xC0) {  // Starts with `110'
		return 2;
	}
	else if ((c & 0xF0) == 0xE0) {  // Starts with `1110'
		return 3;
	}
	else if ((c & 0xF8) == 0xF0) {  // Starts with `11110'
		return 4;
	}
	return 0;
}

/// Return the code point of a UTF-8 character with known length
static inline uint32_t
parse_counted_utf8_char(const uint8_t* utf8, size_t size)
{
	uint32_t c = utf8[0] & ((1u << (8 - size)) - 1);
	for (size_t i = 1; i < size; ++i) {
		const uint8_t in = utf8[i] & 0x3F;
		c = (c << 6) | in;
	}
	return c;
}

/// Parse a UTF-8 character, set *size to the length, and return the code point
static inline uint32_t
parse_utf8_char(const uint8_t* utf8, size_t* size)
{
	switch (*size = utf8_num_bytes(utf8[0])) {
	case 1: case 2: case 3: case 4:
		return parse_counted_utf8_char(utf8, *size);
	default:
		*size = 0;
		return 0;
	}
}

/* URI utilities */

static inline bool
chunk_equals(const SerdChunk* a, const SerdChunk* b)
{
	return a->len == b->len
		&& !strncmp((const char*)a->buf, (const char*)b->buf, a->len);
}

static inline size_t
uri_path_len(const SerdURI* uri)
{
	return uri->path_base.len + uri->path.len;
}

static inline uint8_t
uri_path_at(const SerdURI* uri, size_t i)
{
	if (i < uri->path_base.len) {
		return uri->path_base.buf[i];
	}
	else {
		return uri->path.buf[i - uri->path_base.len];
	}
}

/**
   Return the index of the first differing character after the last root slash,
   or zero if `uri` is not under `root`.
*/
static inline size_t
uri_rooted_index(const SerdURI* uri, const SerdURI* root)
{
	if (!root || !root->scheme.len ||
		!chunk_equals(&root->scheme, &uri->scheme) ||
		!chunk_equals(&root->authority, &uri->authority)) {
		return 0;
	}

	bool         differ = false;
	const size_t path_len = uri_path_len(uri);
	const size_t root_len = uri_path_len(root);
	size_t       last_root_slash = 0;
	for (size_t i = 0; i < path_len && i < root_len; ++i) {
		const uint8_t u = uri_path_at(uri, i);
		const uint8_t r = uri_path_at(root, i);

		differ = differ || u != r;
		if (r == '/') {
			last_root_slash = i;
			if (differ) {
				return 0;
			}
		}
	}

	return last_root_slash + 1;
}

/** Return true iff `uri` shares path components with `root` */
static inline bool
uri_is_related(const SerdURI* uri, const SerdURI* root)
{
	return uri_rooted_index(uri, root) > 0;
}

/** Return true iff `uri` is within the base of `root` */
static inline bool
uri_is_under(const SerdURI* uri, const SerdURI* root)
{
	const size_t index = uri_rooted_index(uri, root);
	return index > 0 && uri->path.len > index;
}

static inline bool
is_uri_scheme_char(const int c)
{
	switch (c) {
	case ':': case '+': case '-': case '.':
		return true;
	default:
		return is_alpha(c) || is_digit(c);
	}
}

/* Error reporting */

static inline void
serd_error(SerdErrorSink error_sink, void* handle, const SerdError* e)
{
	if (error_sink) {
		error_sink(handle, e);
	}
	else {
		fprintf(stderr, "error: %s:%u:%u: ", e->filename, e->line, e->col);
		vfprintf(stderr, e->fmt, *e->args);
	}
}

SERD_LOG_FUNC(3, 4)
int
r_err(SerdReader* reader, SerdStatus st, const char* fmt, ...);

/* Reader */

#ifdef SERD_STACK_CHECK
#    define SERD_STACK_ASSERT_TOP(reader, ref) \
            assert(ref == reader->allocs[reader->n_allocs - 1]);
#else
#    define SERD_STACK_ASSERT_TOP(reader, ref)
#endif

/* Reference to a node in the stack (we can not use pointers since the
   stack may be reallocated, invalidating any pointers to elements).
*/
typedef size_t Ref;

typedef struct {
	Ref                 graph;
	Ref                 subject;
	Ref                 predicate;
	Ref                 object;
	Ref                 datatype;
	Ref                 lang;
	SerdStatementFlags* flags;
} ReadContext;

struct SerdReaderImpl {
	void* handle;
	void              (*free_handle)(void* ptr);
	SerdBaseSink      base_sink;
	SerdPrefixSink    prefix_sink;
	SerdStatementSink statement_sink;
	SerdEndSink       end_sink;
	SerdErrorSink     error_sink;
	void* error_handle;
	Ref               rdf_first;
	Ref               rdf_rest;
	Ref               rdf_nil;
	SerdNode          default_graph;
	SerdByteSource    source;
	SerdStack         stack;
	SerdSyntax        syntax;
	unsigned          next_id;
	SerdStatus        status;
	uint8_t* buf;
	uint8_t* bprefix;
	size_t            bprefix_len;
	bool              strict;     ///< True iff strict parsing
	bool              seen_genid;
#ifdef SERD_STACK_CHECK
	Ref* allocs;     ///< Stack of push offsets
	size_t            n_allocs;   ///< Number of stack pushes
#endif
};

Ref push_node_padded(SerdReader* reader,
	size_t      maxlen,
	SerdType    type,
	const char* str,
	size_t      n_bytes);

Ref push_node(SerdReader* reader,
	SerdType    type,
	const char* str,
	size_t      n_bytes);

size_t genid_size(SerdReader* reader);
Ref    blank_id(SerdReader* reader);
void   set_blank_id(SerdReader* reader, Ref ref, size_t buf_size);

SerdNode* deref(SerdReader* reader, Ref ref);

Ref pop_node(SerdReader* reader, Ref ref);

bool emit_statement(SerdReader* reader, ReadContext ctx, Ref o, Ref d, Ref l);

bool read_n3_statement(SerdReader* reader);
bool read_nquadsDoc(SerdReader* reader);
bool read_turtleTrigDoc(SerdReader* reader);

typedef enum {
	FIELD_NONE,
	FIELD_SUBJECT,
	FIELD_PREDICATE,
	FIELD_OBJECT,
	FIELD_GRAPH
} Field;


static inline int
peek_byte(SerdReader* reader)
{
	SerdByteSource* source = &reader->source;

	return source->eof ? EOF : (int)source->read_buf[source->read_head];
}

static inline int
eat_byte(SerdReader* reader)
{
	const int        c = peek_byte(reader);
	const SerdStatus st = serd_byte_source_advance(&reader->source);
	if (st) {
		reader->status = st;
	}
	return c;
}

static inline int
eat_byte_safe(SerdReader* reader, const int byte)
{
	(void)byte;

	const int c = eat_byte(reader);
	assert(c == byte);
	return c;
}

static inline int
eat_byte_check(SerdReader* reader, const int byte)
{
	const int c = peek_byte(reader);
	if (c != byte) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"expected `%c', not `%c'\n", byte, c);
	}
	return eat_byte_safe(reader, byte);
}

static inline bool
eat_string(SerdReader* reader, const char* str, unsigned n)
{
	bool bad = false;
	for (unsigned i = 0; i < n; ++i) {
		bad |= (bool)eat_byte_check(reader, ((const uint8_t*)str)[i]);
	}
	return bad;
}

static inline SerdStatus
push_byte(SerdReader* reader, Ref ref, const int c)
{
	assert(c != EOF);
	SERD_STACK_ASSERT_TOP(reader, ref);

	uint8_t* const  s = serd_stack_push(&reader->stack, 1);
	SerdNode* const node = (SerdNode*)(reader->stack.buf + ref);
	++node->n_bytes;
	if (!(c & 0x80)) {  // Starts with 0 bit, start of new character
		++node->n_chars;
	}
	*(s - 1) = (uint8_t)c;
	*s = '\0';
	return SERD_SUCCESS;
}

static inline void
push_bytes(SerdReader* reader, Ref ref, const uint8_t* bytes, unsigned len)
{
	for (unsigned i = 0; i < len; ++i) {
		push_byte(reader, ref, bytes[i]);
	}
}

/* Stack */


void
serd_free(void* ptr)
{
	free(ptr);
}

const uint8_t*
serd_strerror(SerdStatus status)
{
	switch (status) {
	case SERD_SUCCESS:        return (const uint8_t*)"Success";
	case SERD_FAILURE:        return (const uint8_t*)"Non-fatal failure";
	case SERD_ERR_UNKNOWN:    return (const uint8_t*)"Unknown error";
	case SERD_ERR_BAD_SYNTAX: return (const uint8_t*)"Invalid syntax";
	case SERD_ERR_BAD_ARG:    return (const uint8_t*)"Invalid argument";
	case SERD_ERR_NOT_FOUND:  return (const uint8_t*)"Not found";
	case SERD_ERR_ID_CLASH:   return (const uint8_t*)"Blank node ID clash";
	case SERD_ERR_BAD_CURIE:  return (const uint8_t*)"Invalid CURIE";
	case SERD_ERR_INTERNAL:   return (const uint8_t*)"Internal error";
	}
	return (const uint8_t*)"Unknown error";  // never reached
}

static inline void
serd_update_flags(const uint8_t c, SerdNodeFlags* const flags)
{
	switch (c) {
	case '\r': case '\n':
		*flags |= SERD_HAS_NEWLINE;
		break;
	case '"':
		*flags |= SERD_HAS_QUOTE;
	}
}

size_t
serd_substrlen(const uint8_t* const str,
	const size_t         len,
	size_t* const        n_bytes,
	SerdNodeFlags* const flags)
{
	size_t        n_chars = 0;
	size_t        i = 0;
	SerdNodeFlags f = 0;
	for (; i < len && str[i]; ++i) {
		if ((str[i] & 0xC0) != 0x80) {  // Start of new character
			++n_chars;
			serd_update_flags(str[i], &f);
		}
	}
	if (n_bytes) {
		*n_bytes = i;
	}
	if (flags) {
		*flags = f;
	}
	return n_chars;
}

size_t
serd_strlen(const uint8_t* str, size_t* n_bytes, SerdNodeFlags* flags)
{
	size_t        n_chars = 0;
	size_t        i = 0;
	SerdNodeFlags f = 0;
	for (; str[i]; ++i) {
		if ((str[i] & 0xC0) != 0x80) {  // Start of new character
			++n_chars;
			serd_update_flags(str[i], &f);
		}
	}
	if (n_bytes) {
		*n_bytes = i;
	}
	if (flags) {
		*flags = f;
	}
	return n_chars;
}

static inline double
read_sign(const char** sptr)
{
	double sign = 1.0;
	switch (**sptr) {
	case '-':
		sign = -1.0;
		// fallthru
	case '+':
		++(*sptr);
		// fallthru
	default:
		return sign;
	}
}

double
serd_strtod(const char* str, char** endptr)
{
	double result = 0.0;

	// Point s at the first non-whitespace character
	const char* s = str;
	while (is_space(*s)) { ++s; }

	// Read leading sign if necessary
	const double sign = read_sign(&s);

	// Parse integer part
	for (; is_digit(*s); ++s) {
		result = (result * 10.0) + (*s - '0');
	}

	// Parse fractional part
	if (*s == '.') {
		double denom = 10.0;
		for (++s; is_digit(*s); ++s) {
			result += (*s - '0') / denom;
			denom *= 10.0;
		}
	}

	// Parse exponent
	if (*s == 'e' || *s == 'E') {
		++s;
		double expt = 0.0;
		double expt_sign = read_sign(&s);
		for (; is_digit(*s); ++s) {
			expt = (expt * 10.0) + (*s - '0');
		}
		result *= pow(10, expt * expt_sign);
	}

	if (endptr) {
		*endptr = (char*)s;
	}

	return result * sign;
}

/**
   Base64 decoding table.
   This is indexed by encoded characters and returns the numeric value used
   for decoding, shifted up by 47 to be in the range of printable ASCII.
   A '$' is a placeholder for characters not in the base64 alphabet.
*/
static const char b64_unmap[] =
"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$m$$$ncdefghijkl$$$$$$"
"$/0123456789:;<=>?@ABCDEFGH$$$$$$IJKLMNOPQRSTUVWXYZ[\\]^_`ab$$$$"
"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$"
"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$";

static inline uint8_t
unmap(const uint8_t in)
{
	return (uint8_t)(b64_unmap[in] - 47);
}

/**
   Decode 4 base64 characters to 3 raw bytes.
*/
static inline size_t
decode_chunk(const uint8_t in[4], uint8_t out[3])
{
	out[0] = (uint8_t)(((unmap(in[0]) << 2)) | unmap(in[1]) >> 4);
	out[1] = (uint8_t)(((unmap(in[1]) << 4) & 0xF0) | unmap(in[2]) >> 2);
	out[2] = (uint8_t)(((unmap(in[2]) << 6) & 0xC0) | unmap(in[3]));
	return 1u + (in[2] != '=') + ((in[2] != '=') && (in[3] != '='));
}

void*
serd_base64_decode(const uint8_t* str, size_t len, size_t* size)
{
	void* buf = malloc((len * 3) / 4 + 2);
	*size = 0;
	for (size_t i = 0, j = 0; i < len; j += 3) {
		uint8_t in[] = "====";
		size_t  n_in = 0;
		for (; i < len && n_in < 4; ++n_in) {
			for (; i < len && !is_base64(str[i]); ++i) {}  // Skip junk
			in[n_in] = str[i++];
		}
		if (n_in > 1) {
			*size += decode_chunk(in, (uint8_t*)buf + j);
		}
	}
	return buf;
}

const uint8_t*
serd_uri_to_path(const uint8_t* uri)
{
	const uint8_t* path = uri;
	if (!is_windows_path(uri) && serd_uri_string_has_scheme(uri)) {
		if (strncmp((const char*)uri, "file:", 5)) {
			fprintf(stderr, "Non-file URI `%s'\n", uri);
			return NULL;
		}
		else if (!strncmp((const char*)uri, "file://localhost/", 17)) {
			path = uri + 16;
		}
		else if (!strncmp((const char*)uri, "file://", 7)) {
			path = uri + 7;
		}
		else {
			fprintf(stderr, "Invalid file URI `%s'\n", uri);
			return NULL;
		}
		if (is_windows_path(path + 1)) {
			++path;  // Special case for terrible Windows file URIs
		}
	}
	return path;
}

uint8_t*
serd_file_uri_parse(const uint8_t* uri, uint8_t** hostname)
{
	const uint8_t* path = uri;
	if (hostname) {
		*hostname = NULL;
	}
	if (!strncmp((const char*)uri, "file://", 7)) {
		const uint8_t* auth = uri + 7;
		if (*auth == '/') {  // No hostname
			path = auth;
		}
		else {  // Has hostname
			if (!(path = (const uint8_t*)strchr((const char*)auth, '/'))) {
				return NULL;
			}
			if (hostname) {
				*hostname = (uint8_t*)calloc((size_t)(path - auth + 1), 1);
				memcpy(*hostname, auth, (size_t)(path - auth));
			}
		}
	}

	if (is_windows_path(path + 1)) {
		++path;
	}

	SerdChunk chunk = { NULL, 0 };
	for (const uint8_t* s = path; *s; ++s) {
		if (*s == '%') {
			if (*(s + 1) == '%') {
				serd_chunk_sink("%", 1, &chunk);
				++s;
			}
			else if (is_hexdig(*(s + 1)) && is_hexdig(*(s + 2))) {
				const uint8_t code[3] = { *(s + 1), *(s + 2), 0 };
				const uint8_t c = (uint8_t)strtoul((const char*)code, NULL, 16);
				serd_chunk_sink(&c, 1, &chunk);
				s += 2;
			}
			else {
				s += 2;  // Junk escape, ignore
			}
		}
		else {
			serd_chunk_sink(s, 1, &chunk);
		}
	}
	return serd_chunk_sink_finish(&chunk);
}

bool
serd_uri_string_has_scheme(const uint8_t* utf8)
{
	// RFC3986: scheme ::= ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
	if (!utf8 || !is_alpha(utf8[0])) {
		return false;  // Invalid scheme initial character, URI is relative
	}

	for (uint8_t c; (c = *++utf8) != '\0';) {
		if (!is_uri_scheme_char(c)) {
			return false;
		}
		else if (c == ':') {
			return true;  // End of scheme
		}
	}

	return false;
}

SerdStatus
serd_uri_parse(const uint8_t* utf8, SerdURI* out)
{
	*out = SERD_URI_NULL;

	const uint8_t* ptr = utf8;

	/* See http://tools.ietf.org/html/rfc3986#section-3
	   URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
	*/

	/* S3.1: scheme ::= ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) */
	if (is_alpha(*ptr)) {
		for (uint8_t c = *++ptr; true; c = *++ptr) {
			switch (c) {
			case '\0': case '/': case '?': case '#':
				ptr = utf8;
				goto path;  // Relative URI (starts with path by definition)
			case ':':
				out->scheme.buf = utf8;
				out->scheme.len = (size_t)((ptr++) - utf8);
				goto maybe_authority;  // URI with scheme
			case '+': case '-': case '.':
				continue;
			default:
				if (is_alpha(c) || is_digit(c)) {
					continue;
				}
			}
		}
	}

	/* S3.2: The authority component is preceded by a double slash ("//")
	   and is terminated by the next slash ("/"), question mark ("?"),
	   or number sign ("#") character, or by the end of the URI.
	*/
maybe_authority:
	if (*ptr == '/' && *(ptr + 1) == '/') {
		ptr += 2;
		out->authority.buf = ptr;
		for (uint8_t c; (c = *ptr) != '\0'; ++ptr) {
			switch (c) {
			case '/': goto path;
			case '?': goto query;
			case '#': goto fragment;
			default:
				++out->authority.len;
			}
		}
	}

	/* RFC3986 S3.3: The path is terminated by the first question mark ("?")
	   or number sign ("#") character, or by the end of the URI.
	*/
path:
	switch (*ptr) {
	case '?':  goto query;
	case '#':  goto fragment;
	case '\0': goto end;
	default:  break;
	}
	out->path.buf = ptr;
	out->path.len = 0;
	for (uint8_t c; (c = *ptr) != '\0'; ++ptr) {
		switch (c) {
		case '?': goto query;
		case '#': goto fragment;
		default:
			++out->path.len;
		}
	}

	/* RFC3986 S3.4: The query component is indicated by the first question
	   mark ("?") character and terminated by a number sign ("#") character
	   or by the end of the URI.
	*/
query:
	if (*ptr == '?') {
		out->query.buf = ++ptr;
		for (uint8_t c; (c = *ptr) != '\0'; ++ptr) {
			if (c == '#') {
				goto fragment;
			}
			++out->query.len;
		}
	}

	/* RFC3986 S3.5: A fragment identifier component is indicated by the
	   presence of a number sign ("#") character and terminated by the end
	   of the URI.
	*/
fragment:
	if (*ptr == '#') {
		out->fragment.buf = ptr;
		while (*ptr++ != '\0') {
			++out->fragment.len;
		}
	}

end:
	return SERD_SUCCESS;
}

/**
   Remove leading dot components from `path`.
   See http://tools.ietf.org/html/rfc3986#section-5.2.3
   @param up Set to the number of up-references (e.g. "../") trimmed
   @return A pointer to the new start of `path`
*/
static const uint8_t*
remove_dot_segments(const uint8_t* path, size_t len, size_t* up)
{
	const uint8_t* begin = path;
	const uint8_t* const end = path + len;

	*up = 0;
	while (begin < end) {
		switch (begin[0]) {
		case '.':
			switch (begin[1]) {
			case '/':
				begin += 2;  // Chop leading "./"
				break;
			case '.':
				switch (begin[2]) {
				case '\0':
					++ * up;
					begin += 2;  // Chop input ".."
					break;
				case '/':
					++ * up;
					begin += 3;  // Chop leading "../"
					break;
				default:
					return begin;
				}
				break;
			case '\0':
				++begin;  // Chop input "."
				// fallthru
			default:
				return begin;
			}
			break;
		case '/':
			switch (begin[1]) {
			case '.':
				switch (begin[2]) {
				case '/':
					begin += 2;  // Leading "/./" => "/"
					break;
				case '.':
					switch (begin[3]) {
					case '/':
						++ * up;
						begin += 3;  // Leading "/../" => "/"
					}
					break;
				default:
					return begin;
				}
			}  // else fall through
		default:
			return begin;  // Finished chopping dot components
		}
	}

	return begin;
}

/// Merge `base` and `path` in-place
static void
merge(SerdChunk* base, SerdChunk* path)
{
	size_t         up;
	const uint8_t* begin = remove_dot_segments(path->buf, path->len, &up);
	const uint8_t* end = path->buf + path->len;

	if (base->len) {
		// Find the up'th last slash
		const uint8_t* base_last = (base->buf + base->len - 1);
		++up;
		do {
			if (*base_last == '/') {
				--up;
			}
		} while (up > 0 && (--base_last > base->buf));

		// Set path prefix
		base->len = (size_t)(base_last - base->buf + 1);
	}

	// Set path suffix
	path->buf = begin;
	path->len = (size_t)(end - begin);
}

/// See http://tools.ietf.org/html/rfc3986#section-5.2.2
void
serd_uri_resolve(const SerdURI* r, const SerdURI* base, SerdURI* t)
{
	if (!base->scheme.len) {
		*t = *r;  // Don't resolve against non-absolute URIs
		return;
	}

	t->path_base.buf = NULL;
	t->path_base.len = 0;
	if (r->scheme.len) {
		*t = *r;
	}
	else {
		if (r->authority.len) {
			t->authority = r->authority;
			t->path = r->path;
			t->query = r->query;
		}
		else {
			t->path = r->path;
			if (!r->path.len) {
				t->path_base = base->path;
				if (r->query.len) {
					t->query = r->query;
				}
				else {
					t->query = base->query;
				}
			}
			else {
				if (r->path.buf[0] != '/') {
					t->path_base = base->path;
				}
				merge(&t->path_base, &t->path);
				t->query = r->query;
			}
			t->authority = base->authority;
		}
		t->scheme = base->scheme;
		t->fragment = r->fragment;
	}
}

/** Write the path of `uri` starting at index `i` */
static size_t
write_path_tail(SerdSink sink, void* stream, const SerdURI* uri, size_t i)
{
	size_t len = 0;
	if (i < uri->path_base.len) {
		len += sink(uri->path_base.buf + i, uri->path_base.len - i, stream);
	}
	if (uri->path.buf) {
		if (i < uri->path_base.len) {
			len += sink(uri->path.buf, uri->path.len, stream);
		}
		else {
			const size_t j = (i - uri->path_base.len);
			len += sink(uri->path.buf + j, uri->path.len - j, stream);
		}
	}
	return len;
}

/** Write the path of `uri` relative to the path of `base`. */
static size_t
write_rel_path(SerdSink       sink,
	void* stream,
	const SerdURI* uri,
	const SerdURI* base)
{
	const size_t path_len = uri_path_len(uri);
	const size_t base_len = uri_path_len(base);
	const size_t min_len = (path_len < base_len) ? path_len : base_len;

	// Find the last separator common to both paths
	size_t last_shared_sep = 0;
	size_t i = 0;
	for (; i < min_len && uri_path_at(uri, i) == uri_path_at(base, i); ++i) {
		if (uri_path_at(uri, i) == '/') {
			last_shared_sep = i;
		}
	}

	if (i == path_len && i == base_len) {  // Paths are identical
		return 0;
	}

	// Find the number of up references ("..") required
	size_t up = 0;
	for (size_t s = last_shared_sep + 1; s < base_len; ++s) {
		if (uri_path_at(base, s) == '/') {
			++up;
		}
	}

	// Write up references
	size_t len = 0;
	for (size_t u = 0; u < up; ++u) {
		len += sink("../", 3, stream);
	}

	if (last_shared_sep == 0 && up == 0) {
		len += sink("/", 1, stream);
	}

	// Write suffix
	return len += write_path_tail(sink, stream, uri, last_shared_sep + 1);
}

static uint8_t
serd_uri_path_starts_without_slash(const SerdURI* uri)
{
	return ((uri->path_base.len || uri->path.len) &&
		((!uri->path_base.len || uri->path_base.buf[0] != '/') &&
			(!uri->path.len || uri->path.buf[0] != '/')));
}

/// See http://tools.ietf.org/html/rfc3986#section-5.3
size_t
serd_uri_serialise_relative(const SerdURI* uri,
	const SerdURI* base,
	const SerdURI* root,
	SerdSink       sink,
	void* stream)
{
	size_t     len = 0;
	const bool relative =
		root ? uri_is_under(uri, root) : uri_is_related(uri, base);

	if (relative) {
		len = write_rel_path(sink, stream, uri, base);
	}
	if (!relative || (!len && base->query.buf)) {
		if (uri->scheme.buf) {
			len += sink(uri->scheme.buf, uri->scheme.len, stream);
			len += sink(":", 1, stream);
		}
		if (uri->authority.buf) {
			len += sink("//", 2, stream);
			len += sink(uri->authority.buf, uri->authority.len, stream);
			if (uri->authority.buf[uri->authority.len - 1] != '/' &&
				serd_uri_path_starts_without_slash(uri)) {
				// Special case: ensure path begins with a slash
				// https://tools.ietf.org/html/rfc3986#section-3.2
				len += sink("/", 1, stream);
			}
		}
		len += write_path_tail(sink, stream, uri, 0);
	}
	if (uri->query.buf) {
		len += sink("?", 1, stream);
		len += sink(uri->query.buf, uri->query.len, stream);
	}
	if (uri->fragment.buf) {
		// Note uri->fragment.buf includes the leading `#'
		len += sink(uri->fragment.buf, uri->fragment.len, stream);
	}
	return len;
}

/// See http://tools.ietf.org/html/rfc3986#section-5.3
size_t
serd_uri_serialise(const SerdURI* uri, SerdSink sink, void* stream)
{
	return serd_uri_serialise_relative(uri, NULL, NULL, sink, stream);
}

typedef struct {
	SerdNode graph;
	SerdNode subject;
	SerdNode predicate;
} WriteContext;

static const WriteContext WRITE_CONTEXT_NULL = {
	{ 0, 0, 0, 0, SERD_NOTHING },
	{ 0, 0, 0, 0, SERD_NOTHING },
	{ 0, 0, 0, 0, SERD_NOTHING }
};

typedef enum {
	SEP_NONE,
	SEP_END_S,       ///< End of a subject ('.')
	SEP_END_P,       ///< End of a predicate (';')
	SEP_END_O,       ///< End of an object (',')
	SEP_S_P,         ///< Between a subject and predicate (whitespace)
	SEP_P_O,         ///< Between a predicate and object (whitespace)
	SEP_ANON_BEGIN,  ///< Start of anonymous node ('[')
	SEP_ANON_END,    ///< End of anonymous node (']')
	SEP_LIST_BEGIN,  ///< Start of list ('(')
	SEP_LIST_SEP,    ///< List separator (whitespace)
	SEP_LIST_END,    ///< End of list (')')
	SEP_GRAPH_BEGIN, ///< Start of graph ('{')
	SEP_GRAPH_END,   ///< End of graph ('}')
	SEP_URI_BEGIN,   ///< URI start quote ('<')
	SEP_URI_END      ///< URI end quote ('>')
} Sep;

typedef struct {
	const char* str;               ///< Sep string
	uint8_t     len;               ///< Length of sep string
	uint8_t     space_before;      ///< Newline before sep
	uint8_t     space_after_node;  ///< Newline after sep if after node
	uint8_t     space_after_sep;   ///< Newline after sep if after sep
} SepRule;

static const SepRule rules[] = {
	{ NULL,     0, 0, 0, 0 },
	{ " .\n\n", 4, 0, 0, 0 },
	{ " ;",     2, 0, 1, 1 },
	{ " ,",     2, 0, 1, 0 },
	{ NULL,     0, 0, 1, 0 },
	{ " ",      1, 0, 0, 0 },
	{ "[",      1, 0, 1, 1 },
	{ "]",      1, 1, 0, 0 },
	{ "(",      1, 0, 0, 0 },
	{ NULL,     1, 0, 1, 0 },
	{ ")",      1, 1, 0, 0 },
	{ " {",     2, 0, 1, 1 },
	{ " }",     2, 0, 1, 1 },
	{ "<",      1, 0, 0, 0 },
	{ ">",      1, 0, 0, 0 },
	{ "\n",     1, 0, 1, 0 }
};

struct SerdWriterImpl {
	SerdSyntax    syntax;
	SerdStyle     style;
	SerdEnv* env;
	SerdNode      root_node;
	SerdURI       root_uri;
	SerdURI       base_uri;
	SerdStack     anon_stack;
	SerdByteSink  byte_sink;
	SerdErrorSink error_sink;
	void* error_handle;
	WriteContext  context;
	SerdNode      list_subj;
	unsigned      list_depth;
	unsigned      indent;
	uint8_t* bprefix;
	size_t        bprefix_len;
	Sep           last_sep;
	bool          empty;
};

typedef enum {
	WRITE_STRING,
	WRITE_LONG_STRING
} TextContext;

static bool
write_node(SerdWriter* writer,
	const SerdNode* node,
	const SerdNode* datatype,
	const SerdNode* lang,
	Field              field,
	SerdStatementFlags flags);

static bool
supports_abbrev(const SerdWriter* writer)
{
	return writer->syntax == SERD_TURTLE || writer->syntax == SERD_TRIG;
}

static void
w_err(SerdWriter* writer, SerdStatus st, const char* fmt, ...)
{
	/* TODO: This results in errors with no file information, which is not
	   helpful when re-serializing a file (particularly for "undefined
	   namespace prefix" errors.  The statement sink API needs to be changed to
	   add a Cursor parameter so the source can notify the writer of the
	   statement origin for better error reporting. */

	va_list args;
	va_start(args, fmt);
	const SerdError e = { st, (const uint8_t*)"", 0, 0, fmt, &args };
	serd_error(writer->error_sink, writer->error_handle, &e);
	va_end(args);
}

static inline WriteContext*
anon_stack_top(SerdWriter* writer)
{
	assert(!serd_stack_is_empty(&writer->anon_stack));
	return (WriteContext*)(writer->anon_stack.buf
		+ writer->anon_stack.size - sizeof(WriteContext));
}

static void
copy_node(SerdNode* dst, const SerdNode* src)
{
	if (src) {
		dst->buf = (uint8_t*)realloc((char*)dst->buf, src->n_bytes + 1);
		dst->n_bytes = src->n_bytes;
		dst->n_chars = src->n_chars;
		dst->flags = src->flags;
		dst->type = src->type;
		memcpy((char*)dst->buf, src->buf, src->n_bytes + 1);
	}
	else {
		dst->type = SERD_NOTHING;
	}
}

static inline size_t
sink(const void* buf, size_t len, SerdWriter* writer)
{
	return serd_byte_sink_write(buf, len, &writer->byte_sink);
}

// Write a single character, as an escape for single byte characters
// (Caller prints any single byte characters that don't need escaping)
static size_t
write_character(SerdWriter* writer, const uint8_t* utf8, size_t* size)
{
	char           escape[11] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	const uint32_t c = parse_utf8_char(utf8, size);
	switch (*size) {
	case 0:
		w_err(writer, SERD_ERR_BAD_ARG, "invalid UTF-8: %X\n", utf8[0]);
		return sink(replacement_char, sizeof(replacement_char), writer);
	case 1:
		snprintf(escape, sizeof(escape), "\\u%04X", utf8[0]);
		return sink(escape, 6, writer);
	default:
		break;
	}

	if (!(writer->style & SERD_STYLE_ASCII)) {
		// Write UTF-8 character directly to UTF-8 output
		return sink(utf8, *size, writer);
	}

	if (c <= 0xFFFF) {
		snprintf(escape, sizeof(escape), "\\u%04X", c);
		return sink(escape, 6, writer);
	}
	else {
		snprintf(escape, sizeof(escape), "\\U%08X", c);
		return sink(escape, 10, writer);
	}
}

static inline bool
uri_must_escape(const uint8_t c)
{
	switch (c) {
	case ' ': case '"': case '<': case '>': case '\\':
	case '^': case '`': case '{': case '|': case '}':
		return true;
	default:
		return !in_range(c, 0x20, 0x7E);
	}
}

static size_t
write_uri(SerdWriter* writer, const uint8_t* utf8, size_t n_bytes)
{
	size_t len = 0;
	for (size_t i = 0; i < n_bytes;) {
		size_t j = i;  // Index of next character that must be escaped
		for (; j < n_bytes; ++j) {
			if (uri_must_escape(utf8[j])) {
				break;
			}
		}

		// Bulk write all characters up to this special one
		len += sink(&utf8[i], j - i, writer);
		if ((i = j) == n_bytes) {
			break;  // Reached end
		}

		// Write UTF-8 character
		size_t size = 0;
		len += write_character(writer, utf8 + i, &size);
		i += size;
		if (size == 0) {
			// Corrupt input, scan to start of next character
			for (++i; i < n_bytes && (utf8[i] & 0x80); ++i) {}
		}
	}
	return len;
}

static bool
lname_must_escape(const uint8_t c)
{
	/* This arbitrary list of characters, most of which have nothing to do with
	   Turtle, must be handled as special cases here because the RDF and SPARQL
	   WGs are apparently intent on making the once elegant Turtle a baroque
	   and inconsistent mess, throwing elegance and extensibility completely
	   out the window for no good reason.

	   Note '-', '.', and '_' are also in PN_LOCAL_ESC, but are valid unescaped
	   in local names, so they are not escaped here. */

	switch (c) {
	case '\'': case '!': case '#': case '$': case '%': case '&':
	case '(': case ')': case '*': case '+': case ',': case '/':
	case ';': case '=': case '?': case '@': case '~':
		return true;
	}
	return false;
}

static size_t
write_lname(SerdWriter* writer, const uint8_t* utf8, size_t n_bytes)
{
	size_t len = 0;
	for (size_t i = 0; i < n_bytes; ++i) {
		size_t j = i;  // Index of next character that must be escaped
		for (; j < n_bytes; ++j) {
			if (lname_must_escape(utf8[j])) {
				break;
			}
		}

		// Bulk write all characters up to this special one
		len += sink(&utf8[i], j - i, writer);
		if ((i = j) == n_bytes) {
			break;  // Reached end
		}

		// Write escape
		len += sink("\\", 1, writer);
		len += sink(&utf8[i], 1, writer);
	}
	return len;
}

static size_t
write_text(SerdWriter* writer, TextContext ctx,
	const uint8_t* utf8, size_t n_bytes)
{
	size_t len = 0;
	for (size_t i = 0; i < n_bytes;) {
		// Fast bulk write for long strings of printable ASCII
		size_t j = i;
		for (; j < n_bytes; ++j) {
			if (utf8[j] == '\\' || utf8[j] == '"'
				|| (!in_range(utf8[j], 0x20, 0x7E))) {
				break;
			}
		}

		len += sink(&utf8[i], j - i, writer);
		if ((i = j) == n_bytes) {
			break;  // Reached end
		}

		const uint8_t in = utf8[i++];
		if (ctx == WRITE_LONG_STRING) {
			switch (in) {
			case '\\': len += sink("\\\\", 2, writer); continue;
			case '\b': len += sink("\\b", 2, writer);  continue;
			case '\n': case '\r': case '\t': case '\f':
				len += sink(&in, 1, writer);  // Write character as-is
				continue;
			case '\"':
				if (i == n_bytes) {  // '"' at string end
					len += sink("\\\"", 2, writer);
				}
				else {
					len += sink(&in, 1, writer);
				}
				continue;
			default: break;
			}
		}
		else if (ctx == WRITE_STRING) {
			switch (in) {
			case '\\': len += sink("\\\\", 2, writer); continue;
			case '\n': len += sink("\\n", 2, writer);  continue;
			case '\r': len += sink("\\r", 2, writer);  continue;
			case '\t': len += sink("\\t", 2, writer);  continue;
			case '"':  len += sink("\\\"", 2, writer); continue;
			default: break;
			}
			if (writer->syntax == SERD_TURTLE) {
				switch (in) {
				case '\b': len += sink("\\b", 2, writer); continue;
				case '\f': len += sink("\\f", 2, writer); continue;
				}
			}
		}

		// Write UTF-8 character
		size_t size = 0;
		len += write_character(writer, utf8 + i - 1, &size);
		if (size == 0) {
			// Corrupt input, scan to start of next character
			for (; i < n_bytes && (utf8[i] & 0x80); ++i) {}
		}
		else {
			i += size - 1;
		}
	}
	return len;
}

static size_t
uri_sink(const void* buf, size_t len, void* stream)
{
	return write_uri((SerdWriter*)stream, (const uint8_t*)buf, len);
}

static void
write_newline(SerdWriter* writer)
{
	sink("\n", 1, writer);
	for (unsigned i = 0; i < writer->indent; ++i) {
		sink("\t", 1, writer);
	}
}

static bool
write_sep(SerdWriter* writer, const Sep sep)
{
	const SepRule* rule = &rules[sep];
	if (rule->space_before) {
		write_newline(writer);
	}
	if (rule->str) {
		sink(rule->str, rule->len, writer);
	}
	if ((writer->last_sep && rule->space_after_sep) ||
		(!writer->last_sep && rule->space_after_node)) {
		write_newline(writer);
	}
	else if (writer->last_sep && rule->space_after_node) {
		sink(" ", 1, writer);
	}
	writer->last_sep = sep;
	return true;
}

static SerdStatus
reset_context(SerdWriter* writer, bool graph)
{
	if (graph) {
		writer->context.graph.type = SERD_NOTHING;
	}
	writer->context.subject.type = SERD_NOTHING;
	writer->context.predicate.type = SERD_NOTHING;
	writer->empty = false;
	return SERD_SUCCESS;
}

static SerdStatus
free_context(SerdWriter* writer)
{
	serd_node_free(&writer->context.graph);
	serd_node_free(&writer->context.subject);
	serd_node_free(&writer->context.predicate);
	return reset_context(writer, true);
}

static bool
is_inline_start(const SerdWriter* writer, Field field, SerdStatementFlags flags)
{
	return (supports_abbrev(writer) &&
		((field == FIELD_SUBJECT && (flags & SERD_ANON_S_BEGIN)) ||
			(field == FIELD_OBJECT && (flags & SERD_ANON_O_BEGIN))));
}

static bool
write_literal(SerdWriter* writer,
	const SerdNode* node,
	const SerdNode* datatype,
	const SerdNode* lang,
	SerdStatementFlags flags)
{
	if (supports_abbrev(writer) && datatype && datatype->buf) {
		const char* type_uri = (const char*)datatype->buf;
		if (!strncmp(type_uri, NS_XSD, sizeof(NS_XSD) - 1) && (
			!strcmp(type_uri + sizeof(NS_XSD) - 1, "boolean") ||
			!strcmp(type_uri + sizeof(NS_XSD) - 1, "integer"))) {
			sink(node->buf, node->n_bytes, writer);
			return true;
		}
		else if (!strncmp(type_uri, NS_XSD, sizeof(NS_XSD) - 1) &&
			!strcmp(type_uri + sizeof(NS_XSD) - 1, "decimal") &&
			strchr((const char*)node->buf, '.') &&
			node->buf[node->n_bytes - 1] != '.') {
			/* xsd:decimal literals without trailing digits, e.g. "5.", can
			   not be written bare in Turtle.  We could add a 0 which is
			   prettier, but changes the text and breaks round tripping.
			*/
			sink(node->buf, node->n_bytes, writer);
			return true;
		}
	}

	if (supports_abbrev(writer)
		&& (node->flags & (SERD_HAS_NEWLINE | SERD_HAS_QUOTE))) {
		sink("\"\"\"", 3, writer);
		write_text(writer, WRITE_LONG_STRING, node->buf, node->n_bytes);
		sink("\"\"\"", 3, writer);
	}
	else {
		sink("\"", 1, writer);
		write_text(writer, WRITE_STRING, node->buf, node->n_bytes);
		sink("\"", 1, writer);
	}
	if (lang && lang->buf) {
		sink("@", 1, writer);
		sink(lang->buf, lang->n_bytes, writer);
	}
	else if (datatype && datatype->buf) {
		sink("^^", 2, writer);
		return write_node(writer, datatype, NULL, NULL, FIELD_NONE, flags);
	}
	return true;
}

// Return true iff `buf` is a valid prefixed name suffix
static inline bool
is_name(const uint8_t* buf, const size_t len)
{
	// TODO: This is more strict than it should be.
	for (size_t i = 0; i < len; ++i) {
		if (!(is_alpha(buf[i]) || is_digit(buf[i]))) {
			return false;
		}
	}
	return true;
}

static bool
write_uri_node(SerdWriter* const        writer,
	const SerdNode* node,
	const Field              field,
	const SerdStatementFlags flags)
{
	SerdNode  prefix;
	SerdChunk suffix;

	if (is_inline_start(writer, field, flags)) {
		++writer->indent;
		write_sep(writer, SEP_ANON_BEGIN);
		sink("== ", 3, writer);
	}

	const bool has_scheme = serd_uri_string_has_scheme(node->buf);
	if (field == FIELD_PREDICATE && supports_abbrev(writer)
		&& !strcmp((const char*)node->buf, NS_RDF "type")) {
		return sink("a", 1, writer) == 1;
	}
	else if (supports_abbrev(writer)
		&& !strcmp((const char*)node->buf, NS_RDF "nil")) {
		return sink("()", 2, writer) == 2;
	}
	else if (has_scheme && (writer->style & SERD_STYLE_CURIED) &&
		serd_env_qualify(writer->env, node, &prefix, &suffix) &&
		is_name(suffix.buf, suffix.len)) {
		write_uri(writer, prefix.buf, prefix.n_bytes);
		sink(":", 1, writer);
		write_uri(writer, suffix.buf, suffix.len);
		return true;
	}

	write_sep(writer, SEP_URI_BEGIN);
	if (writer->style & SERD_STYLE_RESOLVED) {
		SerdURI in_base_uri, uri, abs_uri;
		serd_env_get_base_uri(writer->env, &in_base_uri);
		serd_uri_parse(node->buf, &uri);
		serd_uri_resolve(&uri, &in_base_uri, &abs_uri);
		bool rooted = uri_is_under(&writer->base_uri, &writer->root_uri);
		SerdURI* root = rooted ? &writer->root_uri : &writer->base_uri;
		if (!uri_is_under(&abs_uri, root) ||
			writer->syntax == SERD_NTRIPLES ||
			writer->syntax == SERD_NQUADS) {
			serd_uri_serialise(&abs_uri, uri_sink, writer);
		}
		else {
			serd_uri_serialise_relative(
				&uri, &writer->base_uri, root, uri_sink, writer);
		}
	}
	else {
		write_uri(writer, node->buf, node->n_bytes);
	}
	write_sep(writer, SEP_URI_END);
	if (is_inline_start(writer, field, flags)) {
		sink(" ;", 2, writer);
		write_newline(writer);
	}
	return true;
}

static bool
write_curie(SerdWriter* const        writer,
	const SerdNode* node,
	const Field              field,
	const SerdStatementFlags flags)
{
	SerdChunk  prefix;
	SerdChunk  suffix;
	SerdStatus st;
	switch (writer->syntax) {
	case SERD_NTRIPLES:
	case SERD_NQUADS:
		if ((st = serd_env_expand(writer->env, node, &prefix, &suffix))) {
			w_err(writer, st, "undefined namespace prefix `%s'\n", node->buf);
			return false;
		}
		write_sep(writer, SEP_URI_BEGIN);
		write_uri(writer, prefix.buf, prefix.len);
		write_uri(writer, suffix.buf, suffix.len);
		write_sep(writer, SEP_URI_END);
		break;
	case SERD_TURTLE:
	case SERD_TRIG:
		if (is_inline_start(writer, field, flags)) {
			++writer->indent;
			write_sep(writer, SEP_ANON_BEGIN);
			sink("== ", 3, writer);
		}
		write_lname(writer, node->buf, node->n_bytes);
		if (is_inline_start(writer, field, flags)) {
			sink(" ;", 2, writer);
			write_newline(writer);
		}
	}
	return true;
}

static bool
write_blank(SerdWriter* const        writer,
	const SerdNode* node,
	const Field              field,
	const SerdStatementFlags flags)
{
	if (supports_abbrev(writer)) {
		if (is_inline_start(writer, field, flags)) {
			++writer->indent;
			return write_sep(writer, SEP_ANON_BEGIN);
		}
		else if (field == FIELD_SUBJECT && (flags & SERD_LIST_S_BEGIN)) {
			assert(writer->list_depth == 0);
			copy_node(&writer->list_subj, node);
			++writer->list_depth;
			++writer->indent;
			return write_sep(writer, SEP_LIST_BEGIN);
		}
		else if (field == FIELD_OBJECT && (flags & SERD_LIST_O_BEGIN)) {
			++writer->indent;
			++writer->list_depth;
			return write_sep(writer, SEP_LIST_BEGIN);
		}
		else if ((field == FIELD_SUBJECT && (flags & SERD_EMPTY_S)) ||
			(field == FIELD_OBJECT && (flags & SERD_EMPTY_O))) {
			return sink("[]", 2, writer) == 2;
		}
	}

	sink("_:", 2, writer);
	if (writer->bprefix && !strncmp((const char*)node->buf,
		(const char*)writer->bprefix,
		writer->bprefix_len)) {
		sink(node->buf + writer->bprefix_len,
			node->n_bytes - writer->bprefix_len,
			writer);
	}
	else {
		sink(node->buf, node->n_bytes, writer);
	}

	return true;
}

static bool
write_node(SerdWriter* writer,
	const SerdNode* node,
	const SerdNode* datatype,
	const SerdNode* lang,
	Field              field,
	SerdStatementFlags flags)
{
	bool ret = false;
	switch (node->type) {
	case SERD_LITERAL:
		ret = write_literal(writer, node, datatype, lang, flags);
		break;
	case SERD_URI:
		ret = write_uri_node(writer, node, field, flags);
		break;
	case SERD_CURIE:
		ret = write_curie(writer, node, field, flags);
		break;
	case SERD_BLANK:
		ret = write_blank(writer, node, field, flags);
	default: break;
	}
	writer->last_sep = SEP_NONE;
	return ret;
}

static inline bool
is_resource(const SerdNode* node)
{
	return node->type > SERD_LITERAL;
}

static void
write_pred(SerdWriter* writer, SerdStatementFlags flags, const SerdNode* pred)
{
	write_node(writer, pred, NULL, NULL, FIELD_PREDICATE, flags);
	write_sep(writer, SEP_P_O);
	copy_node(&writer->context.predicate, pred);
}

static bool
write_list_obj(SerdWriter* writer,
	SerdStatementFlags flags,
	const SerdNode* predicate,
	const SerdNode* object,
	const SerdNode* datatype,
	const SerdNode* lang)
{
	if (!strcmp((const char*)object->buf, NS_RDF "nil")) {
		--writer->indent;
		write_sep(writer, SEP_LIST_END);
		return true;
	}
	else if (!strcmp((const char*)predicate->buf, NS_RDF "first")) {
		write_sep(writer, SEP_LIST_SEP);
		write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
	}
	return false;
}

SerdStatus
serd_writer_write_statement(SerdWriter* writer,
	SerdStatementFlags flags,
	const SerdNode* graph,
	const SerdNode* subject,
	const SerdNode* predicate,
	const SerdNode* object,
	const SerdNode* datatype,
	const SerdNode* lang)
{
	if (!subject || !predicate || !object
		|| !subject->buf || !predicate->buf || !object->buf
		|| !is_resource(subject) || !is_resource(predicate)) {
		return SERD_ERR_BAD_ARG;
	}

#define TRY(write_result) \
	if (!(write_result)) { \
		return SERD_ERR_UNKNOWN; \
	}

	switch (writer->syntax) {
	case SERD_NTRIPLES:
	case SERD_NQUADS:
		TRY(write_node(writer, subject, NULL, NULL, FIELD_SUBJECT, flags));
		sink(" ", 1, writer);
		TRY(write_node(writer, predicate, NULL, NULL, FIELD_PREDICATE, flags));
		sink(" ", 1, writer);
		TRY(write_node(writer, object, datatype, lang, FIELD_OBJECT, flags));
		if (writer->syntax == SERD_NQUADS && graph) {
			sink(" ", 1, writer);
			TRY(write_node(writer, graph, datatype, lang, FIELD_GRAPH, flags));
		}
		sink(" .\n", 3, writer);
		return SERD_SUCCESS;
	default:
		break;
	}

	if ((graph && !serd_node_equals(graph, &writer->context.graph)) ||
		(!graph && writer->context.graph.type)) {
		writer->indent = 0;
		if (writer->context.subject.type) {
			write_sep(writer, SEP_END_S);
		}
		if (writer->context.graph.type) {
			write_sep(writer, SEP_GRAPH_END);
		}

		reset_context(writer, true);
		if (graph) {
			TRY(write_node(writer, graph, datatype, lang, FIELD_GRAPH, flags));
			++writer->indent;
			write_sep(writer, SEP_GRAPH_BEGIN);
			copy_node(&writer->context.graph, graph);
		}
	}

	if ((flags & SERD_LIST_CONT)) {
		if (write_list_obj(writer, flags, predicate, object, datatype, lang)) {
			// Reached end of list
			if (--writer->list_depth == 0 && writer->list_subj.type) {
				reset_context(writer, false);
				serd_node_free(&writer->context.subject);
				writer->context.subject = writer->list_subj;
				writer->list_subj = SERD_NODE_NULL;
			}
			return SERD_SUCCESS;
		}
	}
	else if (serd_node_equals(subject, &writer->context.subject)) {
		if (serd_node_equals(predicate, &writer->context.predicate)) {
			// Abbreviate S P
			if (!(flags & SERD_ANON_O_BEGIN)) {
				++writer->indent;
			}
			write_sep(writer, SEP_END_O);
			write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
			if (!(flags & SERD_ANON_O_BEGIN)) {
				--writer->indent;
			}
		}
		else {
			// Abbreviate S
			Sep sep = writer->context.predicate.type ? SEP_END_P : SEP_S_P;
			write_sep(writer, sep);
			write_pred(writer, flags, predicate);
			write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
		}
	}
	else {
		// No abbreviation
		if (writer->context.subject.type) {
			assert(writer->indent > 0);
			--writer->indent;
			if (serd_stack_is_empty(&writer->anon_stack)) {
				write_sep(writer, SEP_END_S);
			}
		}
		else if (!writer->empty) {
			write_sep(writer, SEP_S_P);
		}

		if (!(flags & SERD_ANON_CONT)) {
			write_node(writer, subject, NULL, NULL, FIELD_SUBJECT, flags);
			++writer->indent;
			write_sep(writer, SEP_S_P);
		}
		else {
			++writer->indent;
		}

		reset_context(writer, false);
		copy_node(&writer->context.subject, subject);

		if (!(flags & SERD_LIST_S_BEGIN)) {
			write_pred(writer, flags, predicate);
		}

		write_node(writer, object, datatype, lang, FIELD_OBJECT, flags);
	}

	if (flags & (SERD_ANON_S_BEGIN | SERD_ANON_O_BEGIN)) {
		WriteContext* ctx = (WriteContext*)serd_stack_push(
			&writer->anon_stack, sizeof(WriteContext));
		*ctx = writer->context;
		WriteContext new_context = {
			serd_node_copy(graph), serd_node_copy(subject), SERD_NODE_NULL };
		if ((flags & SERD_ANON_S_BEGIN)) {
			new_context.predicate = serd_node_copy(predicate);
		}
		writer->context = new_context;
	}
	else {
		copy_node(&writer->context.graph, graph);
		copy_node(&writer->context.subject, subject);
		copy_node(&writer->context.predicate, predicate);
	}

	return SERD_SUCCESS;
}

SerdStatus
serd_writer_end_anon(SerdWriter* writer,
	const SerdNode* node)
{
	if (writer->syntax == SERD_NTRIPLES || writer->syntax == SERD_NQUADS) {
		return SERD_SUCCESS;
	}
	if (serd_stack_is_empty(&writer->anon_stack) || writer->indent == 0) {
		w_err(writer, SERD_ERR_UNKNOWN,
			"unexpected end of anonymous node\n");
		return SERD_ERR_UNKNOWN;
	}
	--writer->indent;
	write_sep(writer, SEP_ANON_END);
	free_context(writer);
	writer->context = *anon_stack_top(writer);
	serd_stack_pop(&writer->anon_stack, sizeof(WriteContext));
	const bool is_subject = serd_node_equals(node, &writer->context.subject);
	if (is_subject) {
		copy_node(&writer->context.subject, node);
		writer->context.predicate.type = SERD_NOTHING;
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_writer_finish(SerdWriter* writer)
{
	if (writer->context.subject.type) {
		write_sep(writer, SEP_END_S);
	}
	if (writer->context.graph.type) {
		write_sep(writer, SEP_GRAPH_END);
	}
	serd_byte_sink_flush(&writer->byte_sink);
	writer->indent = 0;
	return free_context(writer);
}

SerdWriter*
serd_writer_new(SerdSyntax     syntax,
	SerdStyle      style,
	SerdEnv* env,
	const SerdURI* base_uri,
	SerdSink       ssink,
	void* stream)
{
	const WriteContext context = WRITE_CONTEXT_NULL;
	SerdWriter* writer = (SerdWriter*)calloc(1, sizeof(SerdWriter));
	writer->syntax = syntax;
	writer->style = style;
	writer->env = env;
	writer->root_node = SERD_NODE_NULL;
	writer->root_uri = SERD_URI_NULL;
	writer->base_uri = base_uri ? *base_uri : SERD_URI_NULL;
	writer->anon_stack = serd_stack_new(4 * sizeof(WriteContext));
	writer->context = context;
	writer->list_subj = SERD_NODE_NULL;
	writer->empty = true;
	writer->byte_sink = serd_byte_sink_new(
		ssink, stream, (style & SERD_STYLE_BULK) ? SERD_PAGE_SIZE : 1);
	return writer;
}

void
serd_writer_set_error_sink(SerdWriter* writer,
	SerdErrorSink error_sink,
	void* error_handle)
{
	writer->error_sink = error_sink;
	writer->error_handle = error_handle;
}

void
serd_writer_chop_blank_prefix(SerdWriter* writer,
	const uint8_t* prefix)
{
	free(writer->bprefix);
	writer->bprefix_len = 0;
	writer->bprefix = NULL;
	if (prefix) {
		writer->bprefix_len = strlen((const char*)prefix);
		writer->bprefix = (uint8_t*)malloc(writer->bprefix_len + 1);
		memcpy(writer->bprefix, prefix, writer->bprefix_len + 1);
	}
}

SerdStatus
serd_writer_set_base_uri(SerdWriter* writer,
	const SerdNode* uri)
{
	if (!serd_env_set_base_uri(writer->env, uri)) {
		serd_env_get_base_uri(writer->env, &writer->base_uri);

		if (writer->syntax == SERD_TURTLE || writer->syntax == SERD_TRIG) {
			if (writer->context.graph.type || writer->context.subject.type) {
				sink(" .\n\n", 4, writer);
				reset_context(writer, true);
			}
			sink("@base <", 7, writer);
			sink(uri->buf, uri->n_bytes, writer);
			sink("> .\n", 4, writer);
		}
		writer->indent = 0;
		return reset_context(writer, true);
	}
	return SERD_ERR_UNKNOWN;
}

SerdStatus
serd_writer_set_root_uri(SerdWriter* writer,
	const SerdNode* uri)
{
	serd_node_free(&writer->root_node);
	if (uri && uri->buf) {
		writer->root_node = serd_node_copy(uri);
		serd_uri_parse(uri->buf, &writer->root_uri);
	}
	else {
		writer->root_node = SERD_NODE_NULL;
		writer->root_uri = SERD_URI_NULL;
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_writer_set_prefix(SerdWriter* writer,
	const SerdNode* name,
	const SerdNode* uri)
{
	if (!serd_env_set_prefix(writer->env, name, uri)) {
		if (writer->syntax == SERD_TURTLE || writer->syntax == SERD_TRIG) {
			if (writer->context.graph.type || writer->context.subject.type) {
				sink(" .\n\n", 4, writer);
				reset_context(writer, true);
			}
			sink("@prefix ", 8, writer);
			sink(name->buf, name->n_bytes, writer);
			sink(": <", 3, writer);
			write_uri(writer, uri->buf, uri->n_bytes);
			sink("> .\n", 4, writer);
		}
		writer->indent = 0;
		return reset_context(writer, true);
	}
	return SERD_ERR_UNKNOWN;
}

void
serd_writer_free(SerdWriter* writer)
{
	serd_writer_finish(writer);
	serd_stack_free(&writer->anon_stack);
	free(writer->bprefix);
	serd_byte_sink_free(&writer->byte_sink);
	serd_node_free(&writer->root_node);
	free(writer);
}

SerdEnv*
serd_writer_get_env(SerdWriter* writer)
{
	return writer->env;
}

size_t
serd_file_sink(const void* buf, size_t len, void* stream)
{
	return fwrite(buf, 1, len, (FILE*)stream);
}

size_t
serd_chunk_sink(const void* buf, size_t len, void* stream)
{
	SerdChunk* chunk = (SerdChunk*)stream;
	chunk->buf = (uint8_t*)realloc((uint8_t*)chunk->buf, chunk->len + len);
	memcpy((uint8_t*)chunk->buf + chunk->len, buf, len);
	chunk->len += len;
	return len;
}

uint8_t*
serd_chunk_sink_finish(SerdChunk* stream)
{
	serd_chunk_sink("", 1, stream);
	return (uint8_t*)stream->buf;
}

int
r_err(SerdReader* reader, SerdStatus st, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	const Cursor* const cur = &reader->source.cur;
	const SerdError e = { st, cur->filename, cur->line, cur->col, fmt, &args };
	serd_error(reader->error_sink, reader->error_handle, &e);
	va_end(args);
	return 0;
}

void
set_blank_id(SerdReader* reader, Ref ref, size_t buf_size)
{
	SerdNode* node = deref(reader, ref);
	const char* prefix = reader->bprefix ? (const char*)reader->bprefix : "";
	node->n_bytes = node->n_chars = (size_t)snprintf(
		(char*)node->buf, buf_size, "%sb%u", prefix, reader->next_id++);
}

size_t
genid_size(SerdReader* reader)
{
	return reader->bprefix_len + 1 + 10 + 1;  // + "b" + UINT32_MAX + \0
}

Ref
blank_id(SerdReader* reader)
{
	Ref ref = push_node_padded(reader, genid_size(reader), SERD_BLANK, "", 0);
	set_blank_id(reader, ref, genid_size(reader));
	return ref;
}

/** fread-like wrapper for getc (which is faster). */
static size_t
serd_file_read_byte(void* buf, size_t size, size_t nmemb, void* stream)
{
	(void)size;
	(void)nmemb;

	const int c = getc((FILE*)stream);
	if (c == EOF) {
		*((uint8_t*)buf) = 0;
		return 0;
	}
	*((uint8_t*)buf) = (uint8_t)c;
	return 1;
}

Ref
push_node_padded(SerdReader* reader, size_t maxlen,
	SerdType type, const char* str, size_t n_bytes)
{
	void* mem = serd_stack_push_aligned(
		&reader->stack, sizeof(SerdNode) + maxlen + 1, sizeof(SerdNode));

	SerdNode* const node = (SerdNode*)mem;
	node->n_bytes = node->n_chars = n_bytes;
	node->flags = 0;
	node->type = type;
	node->buf = NULL;

	uint8_t* buf = (uint8_t*)(node + 1);
	memcpy(buf, str, n_bytes + 1);

#ifdef SERD_STACK_CHECK
	reader->allocs = realloc(
		reader->allocs, sizeof(reader->allocs) * (++reader->n_allocs));
	reader->allocs[reader->n_allocs - 1] = ((uint8_t*)mem - reader->stack.buf);
#endif
	return (Ref)((uint8_t*)node - reader->stack.buf);
}

Ref
push_node(SerdReader* reader, SerdType type, const char* str, size_t n_bytes)
{
	return push_node_padded(reader, n_bytes, type, str, n_bytes);
}

SerdNode*
deref(SerdReader* reader, const Ref ref)
{
	if (ref) {
		SerdNode* node = (SerdNode*)(reader->stack.buf + ref);
		node->buf = (uint8_t*)node + sizeof(SerdNode);
		return node;
	}
	return NULL;
}

Ref
pop_node(SerdReader* reader, Ref ref)
{
	if (ref && ref != reader->rdf_first && ref != reader->rdf_rest
		&& ref != reader->rdf_nil) {
#ifdef SERD_STACK_CHECK
		SERD_STACK_ASSERT_TOP(reader, ref);
		--reader->n_allocs;
#endif
		SerdNode* const node = deref(reader, ref);
		uint8_t* const  top = reader->stack.buf + reader->stack.size;
		serd_stack_pop_aligned(&reader->stack, (size_t)(top - (uint8_t*)node));
	}
	return 0;
}

bool
emit_statement(SerdReader* reader, ReadContext ctx, Ref o, Ref d, Ref l)
{
	SerdNode* graph = deref(reader, ctx.graph);
	if (!graph && reader->default_graph.buf) {
		graph = &reader->default_graph;
	}
	bool ret = !reader->statement_sink ||
		!reader->statement_sink(
			reader->handle, *ctx.flags, graph,
			deref(reader, ctx.subject), deref(reader, ctx.predicate),
			deref(reader, o), deref(reader, d), deref(reader, l));
	*ctx.flags &= SERD_ANON_CONT | SERD_LIST_CONT;  // Preserve only cont flags
	return ret;
}

static bool
read_statement(SerdReader* reader)
{
	switch (reader->syntax) {
	default: return read_n3_statement(reader);
	}
}

static bool
read_doc(SerdReader* reader)
{
	return ((reader->syntax == SERD_NQUADS) ? read_nquadsDoc(reader)
		: read_turtleTrigDoc(reader));
}

SerdReader*
serd_reader_new(SerdSyntax        syntax,
	void* handle,
	void              (*free_handle)(void*),
	SerdBaseSink      base_sink,
	SerdPrefixSink    prefix_sink,
	SerdStatementSink statement_sink,
	SerdEndSink       end_sink)
{
	SerdReader* me = (SerdReader*)calloc(1, sizeof(SerdReader));
	me->handle = handle;
	me->free_handle = free_handle;
	me->base_sink = base_sink;
	me->prefix_sink = prefix_sink;
	me->statement_sink = statement_sink;
	me->end_sink = end_sink;
	me->default_graph = SERD_NODE_NULL;
	me->stack = serd_stack_new(SERD_PAGE_SIZE);
	me->syntax = syntax;
	me->next_id = 1;
	me->strict = true;

	me->rdf_first = push_node(me, SERD_URI, NS_RDF "first", 48);
	me->rdf_rest = push_node(me, SERD_URI, NS_RDF "rest", 47);
	me->rdf_nil = push_node(me, SERD_URI, NS_RDF "nil", 46);

	return me;
}

void
serd_reader_set_strict(SerdReader* reader, bool strict)
{
	reader->strict = strict;
}

void
serd_reader_set_error_sink(SerdReader* reader,
	SerdErrorSink error_sink,
	void* error_handle)
{
	reader->error_sink = error_sink;
	reader->error_handle = error_handle;
}

void
serd_reader_free(SerdReader* reader)
{
	pop_node(reader, reader->rdf_nil);
	pop_node(reader, reader->rdf_rest);
	pop_node(reader, reader->rdf_first);
	serd_node_free(&reader->default_graph);

#ifdef SERD_STACK_CHECK
	free(reader->allocs);
#endif
	free(reader->stack.buf);
	free(reader->bprefix);
	if (reader->free_handle) {
		reader->free_handle(reader->handle);
	}
	free(reader);
}

void*
serd_reader_get_handle(const SerdReader* reader)
{
	return reader->handle;
}

void
serd_reader_add_blank_prefix(SerdReader* reader,
	const uint8_t* prefix)
{
	free(reader->bprefix);
	reader->bprefix_len = 0;
	reader->bprefix = NULL;
	if (prefix) {
		reader->bprefix_len = strlen((const char*)prefix);
		reader->bprefix = (uint8_t*)malloc(reader->bprefix_len + 1);
		memcpy(reader->bprefix, prefix, reader->bprefix_len + 1);
	}
}

void
serd_reader_set_default_graph(SerdReader* reader,
	const SerdNode* graph)
{
	serd_node_free(&reader->default_graph);
	reader->default_graph = serd_node_copy(graph);
}

SerdStatus
serd_reader_read_file(SerdReader* reader,
	const uint8_t* uri)
{
	uint8_t* const path = serd_file_uri_parse(uri, NULL);
	if (!path) {
		return SERD_ERR_BAD_ARG;
	}

	FILE* fd = serd_fopen((const char*)path, "rb");
	if (!fd) {
		serd_free(path);
		return SERD_ERR_UNKNOWN;
	}

	SerdStatus ret = serd_reader_read_file_handle(reader, fd, path);
	fclose(fd);
	free(path);
	return ret;
}

static SerdStatus
skip_bom(SerdReader* me)
{
	if (serd_byte_source_peek(&me->source) == 0xEF) {
		serd_byte_source_advance(&me->source);
		if (serd_byte_source_peek(&me->source) != 0xBB ||
			serd_byte_source_advance(&me->source) ||
			serd_byte_source_peek(&me->source) != 0xBF ||
			serd_byte_source_advance(&me->source)) {
			r_err(me, SERD_ERR_BAD_SYNTAX, "corrupt byte order mark\n");
			return SERD_ERR_BAD_SYNTAX;
		}
	}

	return SERD_SUCCESS;
}

SerdStatus
serd_reader_start_stream(SerdReader* reader,
	FILE* file,
	const uint8_t* name,
	bool           bulk)
{
	return serd_reader_start_source_stream(
		reader,
		bulk ? (SerdSource)fread : serd_file_read_byte,
		(SerdStreamErrorFunc)ferror,
		file,
		name,
		bulk ? SERD_PAGE_SIZE : 1);
}

SerdStatus
serd_reader_start_source_stream(SerdReader* reader,
	SerdSource          read_func,
	SerdStreamErrorFunc error_func,
	void* stream,
	const uint8_t* name,
	size_t              page_size)
{
	return serd_byte_source_open_source(
		&reader->source, read_func, error_func, stream, name, page_size);
}

static SerdStatus
serd_reader_prepare(SerdReader* reader)
{
	reader->status = serd_byte_source_prepare(&reader->source);
	if (reader->status == SERD_SUCCESS) {
		reader->status = skip_bom(reader);
	}
	else if (reader->status == SERD_FAILURE) {
		reader->source.eof = true;
	}
	else {
		r_err(reader, reader->status, "read error: %s\n", strerror(errno));
	}
	return reader->status;
}

SerdStatus
serd_reader_read_chunk(SerdReader* reader)
{
	SerdStatus st = SERD_SUCCESS;
	if (!reader->source.prepared) {
		st = serd_reader_prepare(reader);
	}
	else if (reader->source.eof) {
		st = serd_byte_source_advance(&reader->source);
	}

	if (peek_byte(reader) == 0) {
		// Skip leading null byte, for reading from a null-delimited socket
		eat_byte_safe(reader, 0);
	}

	return st ? st : read_statement(reader) ? SERD_SUCCESS : SERD_FAILURE;
}

SerdStatus
serd_reader_end_stream(SerdReader* reader)
{
	return serd_byte_source_close(&reader->source);
}

SerdStatus
serd_reader_read_file_handle(SerdReader* reader,
	FILE* file,
	const uint8_t* name)
{
	return serd_reader_read_source(
		reader, (SerdSource)fread, (SerdStreamErrorFunc)ferror,
		file, name, SERD_PAGE_SIZE);
}

SerdStatus
serd_reader_read_source(SerdReader* reader,
	SerdSource          source,
	SerdStreamErrorFunc error,
	void* stream,
	const uint8_t* name,
	size_t              page_size)
{
	SerdStatus st = serd_reader_start_source_stream(
		reader, source, error, stream, name, page_size);

	if (st || (st = serd_reader_prepare(reader))) {
		serd_reader_end_stream(reader);
		return st;
	}
	else if (!read_doc(reader)) {
		serd_reader_end_stream(reader);
		return SERD_ERR_UNKNOWN;
	}

	return serd_reader_end_stream(reader);
}

SerdStatus
serd_reader_read_string(SerdReader* reader, const uint8_t* utf8)
{
	serd_byte_source_open_string(&reader->source, utf8);

	SerdStatus st = serd_reader_prepare(reader);
	if (!st) {
		st = read_doc(reader) ? SERD_SUCCESS : SERD_ERR_UNKNOWN;
	}

	serd_byte_source_close(&reader->source);

	return st;
}

#ifdef _WIN32
#    ifndef isnan
#        define isnan(x) _isnan(x)
#    endif
#    ifndef isinf
#        define isinf(x) (!_finite(x))
#    endif
#endif

SerdNode
serd_node_from_string(SerdType type, const uint8_t* str)
{
	if (!str) {
		return SERD_NODE_NULL;
	}

	uint32_t     flags = 0;
	size_t       buf_n_bytes = 0;
	const size_t buf_n_chars = serd_strlen(str, &buf_n_bytes, &flags);
	SerdNode ret = { str, buf_n_bytes, buf_n_chars, flags, type };
	return ret;
}

SerdNode
serd_node_from_substring(SerdType type, const uint8_t* str, const size_t len)
{
	if (!str) {
		return SERD_NODE_NULL;
	}

	uint32_t     flags = 0;
	size_t       buf_n_bytes = 0;
	const size_t buf_n_chars = serd_substrlen(str, len, &buf_n_bytes, &flags);
	assert(buf_n_bytes <= len);
	SerdNode ret = { str, buf_n_bytes, buf_n_chars, flags, type };
	return ret;
}

SerdNode
serd_node_copy(const SerdNode* node)
{
	if (!node || !node->buf) {
		return SERD_NODE_NULL;
	}

	SerdNode copy = *node;
	uint8_t* buf = (uint8_t*)malloc(copy.n_bytes + 1);
	memcpy(buf, node->buf, copy.n_bytes + 1);
	copy.buf = buf;
	return copy;
}

bool
serd_node_equals(const SerdNode* a, const SerdNode* b)
{
	return (a == b)
		|| (a->type == b->type
			&& a->n_bytes == b->n_bytes
			&& a->n_chars == b->n_chars
			&& ((a->buf == b->buf) || !memcmp((const char*)a->buf,
				(const char*)b->buf,
				a->n_bytes + 1)));
}

static size_t
serd_uri_string_length(const SerdURI* uri)
{
	size_t len = uri->path_base.len;

#define ADD_LEN(field, n_delims) \
	if ((field).len) { len += (field).len + (n_delims); }

	ADD_LEN(uri->path, 1);  // + possible leading `/'
	ADD_LEN(uri->scheme, 1);  // + trailing `:'
	ADD_LEN(uri->authority, 2);  // + leading `//'
	ADD_LEN(uri->query, 1);  // + leading `?'
	ADD_LEN(uri->fragment, 1);  // + leading `#'

	return len + 2;  // + 2 for authority `//'
}

static size_t
string_sink(const void* buf, size_t len, void* stream)
{
	uint8_t** ptr = (uint8_t**)stream;
	memcpy(*ptr, buf, len);
	*ptr += len;
	return len;
}

SerdNode
serd_node_new_uri_from_node(const SerdNode* uri_node,
	const SerdURI* base,
	SerdURI* out)
{
	return (uri_node->type == SERD_URI && uri_node->buf)
		? serd_node_new_uri_from_string(uri_node->buf, base, out)
		: SERD_NODE_NULL;
}

SerdNode
serd_node_new_uri_from_string(const uint8_t* str,
	const SerdURI* base,
	SerdURI* out)
{
	if (!str || str[0] == '\0') {
		// Empty URI => Base URI, or nothing if no base is given
		return base ? serd_node_new_uri(base, NULL, out) : SERD_NODE_NULL;
	}

	SerdURI uri;
	serd_uri_parse(str, &uri);
	return serd_node_new_uri(&uri, base, out);  // Resolve/Serialise
}

static inline bool
is_uri_path_char(const uint8_t c)
{
	if (is_alpha(c) || is_digit(c)) {
		return true;
	}
	switch (c) {
	case '-': case '.': case '_': case '~':	 // unreserved
	case ':': case '@':	 // pchar
	case '/':  // separator
	// sub-delims
	case '!': case '$': case '&': case '\'': case '(': case ')':
	case '*': case '+': case ',': case ';': case '=':
		return true;
	default:
		return false;
	}
}

SerdNode
serd_node_new_file_uri(const uint8_t* path,
	const uint8_t* hostname,
	SerdURI* out,
	bool           escape)
{
	const size_t path_len = strlen((const char*)path);
	const size_t hostname_len = hostname ? strlen((const char*)hostname) : 0;
	const bool   evil = is_windows_path(path);
	size_t       uri_len = 0;
	uint8_t* uri = NULL;

	if (path[0] == '/' || is_windows_path(path)) {
		uri_len = strlen("file://") + hostname_len + evil;
		uri = (uint8_t*)malloc(uri_len + 1);
		snprintf((char*)uri, uri_len + 1, "file://%s%s",
			hostname ? (const char*)hostname : "",
			evil ? "/" : "");
	}

	SerdChunk chunk = { uri, uri_len };
	for (size_t i = 0; i < path_len; ++i) {
		if (evil && path[i] == '\\') {
			serd_chunk_sink("/", 1, &chunk);
		}
		else if (path[i] == '%') {
			serd_chunk_sink("%%", 2, &chunk);
		}
		else if (!escape || is_uri_path_char(path[i])) {
			serd_chunk_sink(path + i, 1, &chunk);
		}
		else {
			char escape_str[4] = { '%', 0, 0, 0 };
			snprintf(escape_str + 1, sizeof(escape_str) - 1, "%X", path[i]);
			serd_chunk_sink(escape_str, 3, &chunk);
		}
	}
	serd_chunk_sink_finish(&chunk);

	if (out) {
		serd_uri_parse(chunk.buf, out);
	}

	return serd_node_from_substring(SERD_URI, chunk.buf, chunk.len);
}

SerdNode
serd_node_new_uri(const SerdURI* uri, const SerdURI* base, SerdURI* out)
{
	SerdURI abs_uri = *uri;
	if (base) {
		serd_uri_resolve(uri, base, &abs_uri);
	}

	const size_t len = serd_uri_string_length(&abs_uri);
	uint8_t* buf = (uint8_t*)malloc(len + 1);
	SerdNode     node = { buf, 0, 0, 0, SERD_URI };
	uint8_t* ptr = buf;
	const size_t actual_len = serd_uri_serialise(&abs_uri, string_sink, &ptr);

	buf[actual_len] = '\0';
	node.n_bytes = actual_len;
	node.n_chars = serd_strlen(buf, NULL, NULL);

	if (out) {
		serd_uri_parse(buf, out);  // TODO: cleverly avoid double parse
	}

	return node;
}

SerdNode
serd_node_new_relative_uri(const SerdURI* uri,
	const SerdURI* base,
	const SerdURI* root,
	SerdURI* out)
{
	const size_t uri_len = serd_uri_string_length(uri);
	const size_t base_len = serd_uri_string_length(base);
	uint8_t* buf = (uint8_t*)malloc(uri_len + base_len + 1);
	SerdNode     node = { buf, 0, 0, 0, SERD_URI };
	uint8_t* ptr = buf;
	const size_t actual_len = serd_uri_serialise_relative(
		uri, base, root, string_sink, &ptr);

	buf[actual_len] = '\0';
	node.n_bytes = actual_len;
	node.n_chars = serd_strlen(buf, NULL, NULL);

	if (out) {
		serd_uri_parse(buf, out);  // TODO: cleverly avoid double parse
	}

	return node;
}

static inline unsigned
serd_digits(double abs)
{
	const double lg = ceil(log10(floor(abs) + 1.0));
	return lg < 1.0 ? 1U : (unsigned)lg;
}

SerdNode
serd_node_new_decimal(double d, unsigned frac_digits)
{
	if (isnan(d) || isinf(d)) {
		return SERD_NODE_NULL;
	}

	const double   abs_d = fabs(d);
	const unsigned int_digits = serd_digits(abs_d);
	char* buf = (char*)calloc(int_digits + frac_digits + 3, 1);
	SerdNode       node = { (const uint8_t*)buf, 0, 0, 0, SERD_LITERAL };
	const double   int_part = floor(abs_d);

	// Point s to decimal point location
	char* s = buf + int_digits;
	if (d < 0.0) {
		*buf = '-';
		++s;
	}

	// Write integer part (right to left)
	char* t = s - 1;
	uint64_t dec = (uint64_t)int_part;
	do {
		*t-- = (char)('0' + dec % 10);
	} while ((dec /= 10) > 0);

	*s++ = '.';

	// Write fractional part (right to left)
	double frac_part = fabs(d - int_part);
	if (frac_part < DBL_EPSILON) {
		*s++ = '0';
		node.n_bytes = node.n_chars = (size_t)(s - buf);
	}
	else {
		uint64_t frac = (uint64_t)llround(frac_part * pow(10.0, (int)frac_digits));
		s += frac_digits - 1;
		unsigned i = 0;

		// Skip trailing zeros
		for (; i < frac_digits - 1 && !(frac % 10); ++i, --s, frac /= 10) {}

		node.n_bytes = node.n_chars = (size_t)(s - buf) + 1u;

		// Write digits from last trailing zero to decimal point
		for (; i < frac_digits; ++i) {
			*s-- = (char)('0' + (frac % 10));
			frac /= 10;
		}
	}

	return node;
}

SerdNode
serd_node_new_integer(int64_t i)
{
	int64_t        abs_i = (i < 0) ? -i : i;
	const unsigned digits = serd_digits((double)abs_i);
	char* buf = (char*)calloc(digits + 2, 1);
	SerdNode       node = { (const uint8_t*)buf, 0, 0, 0, SERD_LITERAL };

	// Point s to the end
	char* s = buf + digits - 1;
	if (i < 0) {
		*buf = '-';
		++s;
	}

	node.n_bytes = node.n_chars = (size_t)(s - buf) + 1u;

	// Write integer part (right to left)
	do {
		*s-- = (char)('0' + (abs_i % 10));
	} while ((abs_i /= 10) > 0);

	return node;
}

/**
   Base64 encoding table.
   @see <a href="http://tools.ietf.org/html/rfc3548#section-3">RFC3986 S3</a>.
*/
static const uint8_t b64_map[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

/**
   Encode 3 raw bytes to 4 base64 characters.
*/
static inline void
encode_chunk(uint8_t out[4], const uint8_t in[3], size_t n_in)
{
	out[0] = b64_map[in[0] >> 2];
	out[1] = b64_map[((in[0] & 0x03) << 4) | ((in[1] & 0xF0) >> 4)];
	out[2] = ((n_in > 1)
		? (b64_map[((in[1] & 0x0F) << 2) | ((in[2] & 0xC0) >> 6)])
		: (uint8_t)'=');
	out[3] = ((n_in > 2) ? b64_map[in[2] & 0x3F] : (uint8_t)'=');
}

SerdNode
serd_node_new_blob(const void* buf, size_t size, bool wrap_lines)
{
	const size_t len = (size + 2) / 3 * 4 + (wrap_lines * ((size - 1) / 57));
	uint8_t* str = (uint8_t*)calloc(len + 2, 1);
	SerdNode     node = { str, len, len, 0, SERD_LITERAL };
	for (size_t i = 0, j = 0; i < size; i += 3, j += 4) {
		uint8_t in[4] = { 0, 0, 0, 0 };
		size_t  n_in = MIN(3, size - i);
		memcpy(in, (const uint8_t*)buf + i, n_in);

		if (wrap_lines && i > 0 && (i % 57) == 0) {
			str[j++] = '\n';
			node.flags |= SERD_HAS_NEWLINE;
		}

		encode_chunk(str + j, in, n_in);
	}
	return node;
}

void
serd_node_free(SerdNode* node)
{
	if (node && node->buf) {
		free((uint8_t*)node->buf);
		node->buf = NULL;
	}
}

#define TRY_THROW(exp) if (!(exp)) goto except;
#define TRY_RET(exp)   if (!(exp)) return 0;

static inline bool
fancy_syntax(const SerdReader* reader)
{
	return reader->syntax == SERD_TURTLE || reader->syntax == SERD_TRIG;
}

static bool
read_collection(SerdReader* reader, ReadContext ctx, Ref* dest);

static bool
read_predicateObjectList(SerdReader* reader, ReadContext ctx, bool* ate_dot);

static inline uint8_t
read_HEX(SerdReader* reader)
{
	const int c = peek_byte(reader);
	if (is_xdigit(c)) {
		return (uint8_t)eat_byte_safe(reader, c);
	}

	return (uint8_t)r_err(reader, SERD_ERR_BAD_SYNTAX,
		"invalid hexadecimal digit `%c'\n", c);
}

// Read UCHAR escape, initial \ is already eaten by caller
static inline bool
read_UCHAR(SerdReader* reader, Ref dest, uint32_t* char_code)
{
	const int b = peek_byte(reader);
	unsigned  length = 0;
	switch (b) {
	case 'U':
		length = 8;
		break;
	case 'u':
		length = 4;
		break;
	default:
		return false;
	}
	eat_byte_safe(reader, b);

	uint8_t buf[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	for (unsigned i = 0; i < length; ++i) {
		if (!(buf[i] = read_HEX(reader))) {
			return false;
		}
	}

	char* endptr = NULL;
	const uint32_t code = (uint32_t)strtoul((const char*)buf, &endptr, 16);
	assert(endptr == (char*)buf + length);

	unsigned size = 0;
	if (code < 0x00000080) {
		size = 1;
	}
	else if (code < 0x00000800) {
		size = 2;
	}
	else if (code < 0x00010000) {
		size = 3;
	}
	else if (code < 0x00110000) {
		size = 4;
	}
	else {
		r_err(reader, SERD_ERR_BAD_SYNTAX,
			"unicode character 0x%X out of range\n", code);
		push_bytes(reader, dest, replacement_char, 3);
		*char_code = 0xFFFD;
		return true;
	}

	// Build output in buf
	// (Note # of bytes = # of leading 1 bits in first byte)
	uint32_t c = code;
	switch (size) {
	case 4:
		buf[3] = (uint8_t)(0x80u | (c & 0x3Fu));
		c >>= 6;
		c |= (16 << 12);  // set bit 4
		// fallthru
	case 3:
		buf[2] = (uint8_t)(0x80u | (c & 0x3Fu));
		c >>= 6;
		c |= (32 << 6);  // set bit 5
		// fallthru
	case 2:
		buf[1] = (uint8_t)(0x80u | (c & 0x3Fu));
		c >>= 6;
		c |= 0xC0;  // set bits 6 and 7
		// fallthru
	case 1:
		buf[0] = (uint8_t)c;
	}

	push_bytes(reader, dest, buf, size);
	*char_code = code;
	return true;
}

// Read ECHAR escape, initial \ is already eaten by caller
static inline bool
read_ECHAR(SerdReader* reader, Ref dest, SerdNodeFlags* flags)
{
	const int c = peek_byte(reader);
	switch (c) {
	case 't':
		eat_byte_safe(reader, 't');
		push_byte(reader, dest, '\t');
		return true;
	case 'b':
		eat_byte_safe(reader, 'b');
		push_byte(reader, dest, '\b');
		return true;
	case 'n':
		*flags |= SERD_HAS_NEWLINE;
		eat_byte_safe(reader, 'n');
		push_byte(reader, dest, '\n');
		return true;
	case 'r':
		*flags |= SERD_HAS_NEWLINE;
		eat_byte_safe(reader, 'r');
		push_byte(reader, dest, '\r');
		return true;
	case 'f':
		eat_byte_safe(reader, 'f');
		push_byte(reader, dest, '\f');
		return true;
	case '\\': case '"': case '\'':
		push_byte(reader, dest, eat_byte_safe(reader, c));
		return true;
	default:
		return false;
	}
}

static inline SerdStatus
bad_char(SerdReader* reader, const char* fmt, uint8_t c)
{
	// Skip bytes until the next start byte
	for (int b = peek_byte(reader); b != EOF && ((uint8_t)b & 0x80);) {
		eat_byte_safe(reader, b);
		b = peek_byte(reader);
	}

	r_err(reader, SERD_ERR_BAD_SYNTAX, fmt, c);
	return reader->strict ? SERD_ERR_BAD_SYNTAX : SERD_FAILURE;
}

static SerdStatus
read_utf8_bytes(SerdReader* reader, uint8_t bytes[4], uint32_t* size, uint8_t c)
{
	*size = utf8_num_bytes(c);
	if (*size <= 1 || *size > 4) {
		return bad_char(reader, "invalid UTF-8 start 0x%X\n", c);
	}

	bytes[0] = c;
	for (unsigned i = 1; i < *size; ++i) {
		const int b = peek_byte(reader);
		if (b == EOF || ((uint8_t)b & 0x80) == 0) {
			return bad_char(reader, "invalid UTF-8 continuation 0x%X\n",
				(uint8_t)b);
		}

		eat_byte_safe(reader, b);
		bytes[i] = (uint8_t)b;
	}

	return SERD_SUCCESS;
}

static SerdStatus
read_utf8_character(SerdReader* reader, Ref dest, uint8_t c)
{
	uint32_t   size;
	uint8_t    bytes[4];
	SerdStatus st = read_utf8_bytes(reader, bytes, &size, c);
	if (st) {
		push_bytes(reader, dest, replacement_char, 3);
	}
	else {
		push_bytes(reader, dest, bytes, size);
	}
	return st;
}

static SerdStatus
read_utf8_code(SerdReader* reader, Ref dest, uint32_t* code, uint8_t c)
{
	uint32_t   size;
	uint8_t    bytes[4] = { 0, 0, 0, 0 };
	SerdStatus st = read_utf8_bytes(reader, bytes, &size, c);
	if (st) {
		push_bytes(reader, dest, replacement_char, 3);
		return st;
	}

	push_bytes(reader, dest, bytes, size);
	*code = parse_counted_utf8_char(bytes, size);
	return st;
}

// Read one character (possibly multi-byte)
// The first byte, c, has already been eaten by caller
static inline SerdStatus
read_character(SerdReader* reader, Ref dest, SerdNodeFlags* flags, uint8_t c)
{
	if (!(c & 0x80)) {
		switch (c) {
		case 0xA: case 0xD:
			*flags |= SERD_HAS_NEWLINE;
			break;
		case '"': case '\'':
			*flags |= SERD_HAS_QUOTE;
			break;
		}
		push_byte(reader, dest, c);
		return SERD_SUCCESS;
	}
	return read_utf8_character(reader, dest, c);
}

// [10] comment ::= '#' ( [^#xA #xD] )*
static void
read_comment(SerdReader* reader)
{
	eat_byte_safe(reader, '#');
	int c;
	while (((c = peek_byte(reader)) != 0xA) && c != 0xD && c != EOF && c) {
		eat_byte_safe(reader, c);
	}
}

// [24] ws ::= #x9 | #xA | #xD | #x20 | comment
static inline bool
read_ws(SerdReader* reader)
{
	const int c = peek_byte(reader);
	switch (c) {
	case 0x9: case 0xA: case 0xD: case 0x20:
		eat_byte_safe(reader, c);
		return true;
	case '#':
		read_comment(reader);
		return true;
	default:
		return false;
	}
}

static inline bool
read_ws_star(SerdReader* reader)
{
	while (read_ws(reader)) {}
	return true;
}

static inline bool
peek_delim(SerdReader* reader, const char delim)
{
	read_ws_star(reader);
	return peek_byte(reader) == delim;
}

static inline bool
eat_delim(SerdReader* reader, const char delim)
{
	if (peek_delim(reader, delim)) {
		eat_byte_safe(reader, delim);
		return read_ws_star(reader);
	}
	return false;
}

// STRING_LITERAL_LONG_QUOTE and STRING_LITERAL_LONG_SINGLE_QUOTE
// Initial triple quotes are already eaten by caller
static Ref
read_STRING_LITERAL_LONG(SerdReader* reader, SerdNodeFlags* flags, uint8_t q)
{
	Ref        ref = push_node(reader, SERD_LITERAL, "", 0);
	SerdStatus st = SERD_SUCCESS;
	while (!reader->status && !(st && reader->strict)) {
		const int c = peek_byte(reader);
		if (c == '\\') {
			eat_byte_safe(reader, c);
			uint32_t code;
			if (!read_ECHAR(reader, ref, flags) &&
				!read_UCHAR(reader, ref, &code)) {
				r_err(reader, SERD_ERR_BAD_SYNTAX,
					"invalid escape `\\%c'\n", peek_byte(reader));
				return pop_node(reader, ref);
			}
		}
		else if (c == q) {
			eat_byte_safe(reader, q);
			const int q2 = eat_byte_safe(reader, peek_byte(reader));
			const int q3 = peek_byte(reader);
			if (q2 == q && q3 == q) {  // End of string
				eat_byte_safe(reader, q3);
				break;
			}
			*flags |= SERD_HAS_QUOTE;
			push_byte(reader, ref, c);
			read_character(reader, ref, flags, (uint8_t)q2);
		}
		else if (c == EOF) {
			r_err(reader, SERD_ERR_BAD_SYNTAX, "end of file in long string\n");
			return pop_node(reader, ref);
		}
		else {
			st = read_character(
				reader, ref, flags, (uint8_t)eat_byte_safe(reader, c));
		}
	}
	return ref;
}

// STRING_LITERAL_QUOTE and STRING_LITERAL_SINGLE_QUOTE
// Initial quote is already eaten by caller
static Ref
read_STRING_LITERAL(SerdReader* reader, SerdNodeFlags* flags, uint8_t q)
{
	Ref        ref = push_node(reader, SERD_LITERAL, "", 0);
	SerdStatus st = SERD_SUCCESS;
	while (!reader->status && !(st && reader->strict)) {
		const int c = peek_byte(reader);
		uint32_t  code = 0;
		switch (c) {
		case EOF:
			r_err(reader, SERD_ERR_BAD_SYNTAX, "end of file in short string\n");
			return pop_node(reader, ref);
		case '\n': case '\r':
			r_err(reader, SERD_ERR_BAD_SYNTAX, "line end in short string\n");
			return pop_node(reader, ref);
		case '\\':
			eat_byte_safe(reader, c);
			if (!read_ECHAR(reader, ref, flags) &&
				!read_UCHAR(reader, ref, &code)) {
				r_err(reader, SERD_ERR_BAD_SYNTAX,
					"invalid escape `\\%c'\n", peek_byte(reader));
				return pop_node(reader, ref);
			}
			break;
		default:
			if (c == q) {
				eat_byte_check(reader, q);
				return ref;
			}
			else {
				st = read_character(
					reader, ref, flags, (uint8_t)eat_byte_safe(reader, c));
			}
		}
	}
	eat_byte_check(reader, q);
	return ref;
}

static Ref
read_String(SerdReader* reader, SerdNodeFlags* flags)
{
	const int q1 = peek_byte(reader);
	eat_byte_safe(reader, q1);

	const int q2 = peek_byte(reader);
	if (q2 == EOF) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected end of file\n");
	}
	else if (q2 != q1) {  // Short string (not triple quoted)
		return read_STRING_LITERAL(reader, flags, (uint8_t)q1);
	}

	eat_byte_safe(reader, q2);
	const int q3 = peek_byte(reader);
	if (q3 == EOF) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected end of file\n");
	}
	else if (q3 != q1) {  // Empty short string ("" or '')
		return push_node(reader, SERD_LITERAL, "", 0);
	}

	if (!fancy_syntax(reader)) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"syntax does not support long literals\n");
	}

	eat_byte_safe(reader, q3);
	return read_STRING_LITERAL_LONG(reader, flags, (uint8_t)q1);
}

static inline bool
is_PN_CHARS_BASE(const uint32_t c)
{
	return ((c >= 0x00C0 && c <= 0x00D6) || (c >= 0x00D8 && c <= 0x00F6) ||
		(c >= 0x00F8 && c <= 0x02FF) || (c >= 0x0370 && c <= 0x037D) ||
		(c >= 0x037F && c <= 0x1FFF) || (c >= 0x200C && c <= 0x200D) ||
		(c >= 0x2070 && c <= 0x218F) || (c >= 0x2C00 && c <= 0x2FEF) ||
		(c >= 0x3001 && c <= 0xD7FF) || (c >= 0xF900 && c <= 0xFDCF) ||
		(c >= 0xFDF0 && c <= 0xFFFD) || (c >= 0x10000 && c <= 0xEFFFF));
}

static SerdStatus
read_PN_CHARS_BASE(SerdReader* reader, Ref dest)
{
	uint32_t   code;
	const int  c = peek_byte(reader);
	SerdStatus st = SERD_SUCCESS;
	if (is_alpha(c)) {
		push_byte(reader, dest, eat_byte_safe(reader, c));
	}
	else if (c == EOF || !(c & 0x80)) {
		return SERD_FAILURE;
	}
	else if ((st = read_utf8_code(reader, dest, &code,
		(uint8_t)eat_byte_safe(reader, c)))) {
		return st;
	}
	else if (!is_PN_CHARS_BASE(code)) {
		r_err(reader, SERD_ERR_BAD_SYNTAX,
			"invalid character U+%04X in name\n", code);
		if (reader->strict) {
			return SERD_ERR_BAD_SYNTAX;
		}
	}
	return st;
}

static inline bool
is_PN_CHARS(const uint32_t c)
{
	return (is_PN_CHARS_BASE(c) || c == 0xB7 ||
		(c >= 0x0300 && c <= 0x036F) || (c >= 0x203F && c <= 0x2040));
}

static SerdStatus
read_PN_CHARS(SerdReader* reader, Ref dest)
{
	uint32_t   code;
	const int  c = peek_byte(reader);
	SerdStatus st = SERD_SUCCESS;
	if (is_alpha(c) || is_digit(c) || c == '_' || c == '-') {
		push_byte(reader, dest, eat_byte_safe(reader, c));
	}
	else if (c == EOF || !(c & 0x80)) {
		return SERD_FAILURE;
	}
	else if ((st = read_utf8_code(reader, dest, &code,
		(uint8_t)eat_byte_safe(reader, c)))) {
		return st;
	}
	else if (!is_PN_CHARS(code)) {
		r_err(reader, (st = SERD_ERR_BAD_SYNTAX),
			"invalid character U+%04X in name\n", code);
	}
	return st;
}

static bool
read_PERCENT(SerdReader* reader, Ref dest)
{
	push_byte(reader, dest, eat_byte_safe(reader, '%'));
	const uint8_t h1 = read_HEX(reader);
	const uint8_t h2 = read_HEX(reader);
	if (h1 && h2) {
		push_byte(reader, dest, h1);
		push_byte(reader, dest, h2);
		return true;
	}
	return false;
}

static SerdStatus
read_PLX(SerdReader* reader, Ref dest)
{
	int c = peek_byte(reader);
	switch (c) {
	case '%':
		if (!read_PERCENT(reader, dest)) {
			return SERD_ERR_BAD_SYNTAX;
		}
		return SERD_SUCCESS;
	case '\\':
		eat_byte_safe(reader, c);
		if (is_alpha(c = peek_byte(reader))) {
			// Escapes like \u \n etc. are not supported
			return SERD_ERR_BAD_SYNTAX;
		}
		// Allow escaping of pretty much any other character
		push_byte(reader, dest, eat_byte_safe(reader, c));
		return SERD_SUCCESS;
	default:
		return SERD_FAILURE;
	}
}

static SerdStatus
read_PN_LOCAL(SerdReader* reader, Ref dest, bool* ate_dot)
{
	int        c = peek_byte(reader);
	SerdStatus st = SERD_SUCCESS;
	bool       trailing_unescaped_dot = false;
	switch (c) {
	case '0': case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9': case ':': case '_':
		push_byte(reader, dest, eat_byte_safe(reader, c));
		break;
	default:
		if ((st = read_PLX(reader, dest)) > SERD_FAILURE) {
			return st;
		}
		else if (st != SERD_SUCCESS && read_PN_CHARS_BASE(reader, dest)) {
			return SERD_FAILURE;
		}
	}

	while ((c = peek_byte(reader))) {  // Middle: (PN_CHARS | '.' | ':')*
		if (c == '.' || c == ':') {
			push_byte(reader, dest, eat_byte_safe(reader, c));
		}
		else if ((st = read_PLX(reader, dest)) > SERD_FAILURE) {
			return st;
		}
		else if (st != SERD_SUCCESS && (st = read_PN_CHARS(reader, dest))) {
			break;
		}
		trailing_unescaped_dot = (c == '.');
	}

	SerdNode* const n = deref(reader, dest);
	if (trailing_unescaped_dot) {
		// Ate trailing dot, pop it from stack/node and inform caller
		--n->n_bytes;
		serd_stack_pop(&reader->stack, 1);
		*ate_dot = true;
	}

	return (st > SERD_FAILURE) ? st : SERD_SUCCESS;
}

// Read the remainder of a PN_PREFIX after some initial characters
static SerdStatus
read_PN_PREFIX_tail(SerdReader* reader, Ref dest)
{
	int c;
	while ((c = peek_byte(reader))) {  // Middle: (PN_CHARS | '.')*
		if (c == '.') {
			push_byte(reader, dest, eat_byte_safe(reader, c));
		}
		else if (read_PN_CHARS(reader, dest)) {
			break;
		}
	}

	const SerdNode* const n = deref(reader, dest);
	if (n->buf[n->n_bytes - 1] == '.' && read_PN_CHARS(reader, dest)) {
		r_err(reader, SERD_ERR_BAD_SYNTAX, "prefix ends with `.'\n");
		return SERD_ERR_BAD_SYNTAX;
	}

	return SERD_SUCCESS;
}

static SerdStatus
read_PN_PREFIX(SerdReader* reader, Ref dest)
{
	if (!read_PN_CHARS_BASE(reader, dest)) {
		return read_PN_PREFIX_tail(reader, dest);
	}
	return SERD_FAILURE;
}

static Ref
read_LANGTAG(SerdReader* reader)
{
	int c = peek_byte(reader);
	if (!is_alpha(c)) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected `%c'\n", c);
	}

	Ref ref = push_node(reader, SERD_LITERAL, "", 0);
	push_byte(reader, ref, eat_byte_safe(reader, c));
	while ((c = peek_byte(reader)) && is_alpha(c)) {
		push_byte(reader, ref, eat_byte_safe(reader, c));
	}
	while (peek_byte(reader) == '-') {
		push_byte(reader, ref, eat_byte_safe(reader, '-'));
		while ((c = peek_byte(reader)) && (is_alpha(c) || is_digit(c))) {
			push_byte(reader, ref, eat_byte_safe(reader, c));
		}
	}
	return ref;
}

static bool
read_IRIREF_scheme(SerdReader* reader, Ref dest)
{
	int c = peek_byte(reader);
	if (!isalpha(c)) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"bad IRI scheme start `%c'\n", c);
	}

	while ((c = peek_byte(reader)) != EOF) {
		if (c == '>') {
			return r_err(reader, SERD_ERR_BAD_SYNTAX, "missing IRI scheme\n");
		}
		else if (!is_uri_scheme_char(c)) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"bad IRI scheme char `%X'\n", c);
		}

		push_byte(reader, dest, eat_byte_safe(reader, c));
		if (c == ':') {
			return true;  // End of scheme
		}
	}

	return r_err(reader, SERD_ERR_BAD_SYNTAX, "unexpected end of file\n");
}

static Ref
read_IRIREF(SerdReader* reader)
{
	TRY_RET(eat_byte_check(reader, '<'));
	Ref        ref = push_node(reader, SERD_URI, "", 0);
	SerdStatus st = SERD_SUCCESS;
	if (!fancy_syntax(reader) && !read_IRIREF_scheme(reader, ref)) {
		return pop_node(reader, ref);
	}

	uint32_t code = 0;
	while (!reader->status && !(st && reader->strict)) {
		const int c = eat_byte_safe(reader, peek_byte(reader));
		switch (c) {
		case '"': case '<': case '^': case '`': case '{': case '|': case '}':
			r_err(reader, SERD_ERR_BAD_SYNTAX,
				"invalid IRI character `%c'\n", c);
			return pop_node(reader, ref);
		case '>':
			return ref;
		case '\\':
			if (!read_UCHAR(reader, ref, &code)) {
				r_err(reader, SERD_ERR_BAD_SYNTAX, "invalid IRI escape\n");
				return pop_node(reader, ref);
			}
			switch (code) {
			case 0: case ' ': case '<': case '>':
				r_err(reader, SERD_ERR_BAD_SYNTAX,
					"invalid escaped IRI character %X %c\n", code, code);
				return pop_node(reader, ref);
			}
			break;
		default:
			if (c <= 0x20) {
				if (isprint(c)) {
					r_err(reader, SERD_ERR_BAD_SYNTAX,
						"invalid IRI character `%c' (escape %%%02X)\n",
						c, (unsigned)c);
				}
				else {
					r_err(reader, SERD_ERR_BAD_SYNTAX,
						"invalid IRI character (escape %%%02X)\n",
						(unsigned)c);
				}
				if (reader->strict) {
					return pop_node(reader, ref);
				}
				reader->status = SERD_FAILURE;
				push_byte(reader, ref, c);
			}
			else if (!(c & 0x80)) {
				push_byte(reader, ref, c);
			}
			else if ((st = read_utf8_character(reader, ref, (uint8_t)c))) {
				if (reader->strict) {
					reader->status = SERD_FAILURE;
					return pop_node(reader, ref);
				}
			}
		}
	}
	return pop_node(reader, ref);
}

static bool
read_PrefixedName(SerdReader* reader, Ref dest, bool read_prefix, bool* ate_dot)
{
	if (read_prefix && read_PN_PREFIX(reader, dest) > SERD_FAILURE) {
		return false;
	}
	else if (peek_byte(reader) != ':') {
		return false;
	}

	push_byte(reader, dest, eat_byte_safe(reader, ':'));
	return read_PN_LOCAL(reader, dest, ate_dot) <= SERD_FAILURE;
}

static bool
read_0_9(SerdReader* reader, Ref str, bool at_least_one)
{
	unsigned count = 0;
	for (int c; is_digit((c = peek_byte(reader))); ++count) {
		push_byte(reader, str, eat_byte_safe(reader, c));
	}
	if (at_least_one && count == 0) {
		r_err(reader, SERD_ERR_BAD_SYNTAX, "expected digit\n");
	}
	return count;
}

static bool
read_number(SerdReader* reader, Ref* dest, Ref* datatype, bool* ate_dot)
{
#define XSD_DECIMAL NS_XSD "decimal"
#define XSD_DOUBLE  NS_XSD "double"
#define XSD_INTEGER NS_XSD "integer"

	Ref  ref = push_node(reader, SERD_LITERAL, "", 0);
	int  c = peek_byte(reader);
	bool has_decimal = false;
	if (c == '-' || c == '+') {
		push_byte(reader, ref, eat_byte_safe(reader, c));
	}
	if ((c = peek_byte(reader)) == '.') {
		has_decimal = true;
		// decimal case 2 (e.g. '.0' or `-.0' or `+.0')
		push_byte(reader, ref, eat_byte_safe(reader, c));
		TRY_THROW(read_0_9(reader, ref, true));
	}
	else {
		// all other cases ::= ( '-' | '+' ) [0-9]+ ( . )? ( [0-9]+ )? ...
		TRY_THROW(is_digit(c));
		read_0_9(reader, ref, true);
		if ((c = peek_byte(reader)) == '.') {
			has_decimal = true;

			// Annoyingly, dot can be end of statement, so tentatively eat
			eat_byte_safe(reader, c);
			c = peek_byte(reader);
			if (!is_digit(c) && c != 'e' && c != 'E') {
				*dest = ref;
				*ate_dot = true;  // Force caller to deal with stupid grammar
				return true;  // Next byte is not a number character, done
			}

			push_byte(reader, ref, '.');
			read_0_9(reader, ref, false);
		}
	}
	c = peek_byte(reader);
	if (c == 'e' || c == 'E') {
		// double
		push_byte(reader, ref, eat_byte_safe(reader, c));
		switch ((c = peek_byte(reader))) {
		case '+': case '-':
			push_byte(reader, ref, eat_byte_safe(reader, c));
		default: break;
		}
		TRY_THROW(read_0_9(reader, ref, true));
		*datatype = push_node(reader, SERD_URI,
			XSD_DOUBLE, sizeof(XSD_DOUBLE) - 1);
	}
	else if (has_decimal) {
		*datatype = push_node(reader, SERD_URI,
			XSD_DECIMAL, sizeof(XSD_DECIMAL) - 1);
	}
	else {
		*datatype = push_node(reader, SERD_URI,
			XSD_INTEGER, sizeof(XSD_INTEGER) - 1);
	}
	*dest = ref;
	return true;
except:
	pop_node(reader, *datatype);
	pop_node(reader, ref);
	return r_err(reader, SERD_ERR_BAD_SYNTAX, "bad number syntax\n");
}

static bool
read_iri(SerdReader* reader, Ref* dest, bool* ate_dot)
{
	switch (peek_byte(reader)) {
	case '<':
		*dest = read_IRIREF(reader);
		return true;
	default:
		*dest = push_node(reader, SERD_CURIE, "", 0);
		return read_PrefixedName(reader, *dest, true, ate_dot);
	}
}

static bool
read_literal(SerdReader* reader, Ref* dest,
	Ref* datatype, Ref* lang, SerdNodeFlags* flags, bool* ate_dot)
{
	Ref str = read_String(reader, flags);
	if (!str) {
		return false;
	}

	switch (peek_byte(reader)) {
	case '@':
		eat_byte_safe(reader, '@');
		TRY_THROW(*lang = read_LANGTAG(reader));
		break;
	case '^':
		eat_byte_safe(reader, '^');
		eat_byte_check(reader, '^');
		TRY_THROW(read_iri(reader, datatype, ate_dot));
		break;
	}
	*dest = str;
	return true;
except:
	*datatype = pop_node(reader, *datatype);
	*lang = pop_node(reader, *lang);
	pop_node(reader, str);
	return r_err(reader, SERD_ERR_BAD_SYNTAX, "bad literal syntax\n");
}

static bool
read_verb(SerdReader* reader, Ref* dest)
{
	if (peek_byte(reader) == '<') {
		return (*dest = read_IRIREF(reader));
	}

	/* Either a qname, or "a".  Read the prefix first, and if it is in fact
	   "a", produce that instead.
	*/
	*dest = push_node(reader, SERD_CURIE, "", 0);
	const SerdStatus st = read_PN_PREFIX(reader, *dest);
	bool             ate_dot = false;
	SerdNode* node = deref(reader, *dest);
	const int        next = peek_byte(reader);
	if (!st && node->n_bytes == 1 && node->buf[0] == 'a' &&
		next != ':' && !is_PN_CHARS_BASE((uint32_t)next)) {
		pop_node(reader, *dest);
		return (*dest = push_node(reader, SERD_URI, NS_RDF "type", 47));
	}
	else if (st > SERD_FAILURE ||
		!read_PrefixedName(reader, *dest, false, &ate_dot) ||
		ate_dot) {
		*dest = pop_node(reader, *dest);
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "bad verb\n");
	}

	return true;
}

static Ref
read_BLANK_NODE_LABEL(SerdReader* reader, bool* ate_dot)
{
	eat_byte_safe(reader, '_');
	eat_byte_check(reader, ':');
	Ref ref = push_node(reader, SERD_BLANK,
		reader->bprefix ? (char*)reader->bprefix : "",
		reader->bprefix_len);

	int c = peek_byte(reader);  // First: (PN_CHARS | '_' | [0-9])
	if (is_digit(c) || c == '_') {
		push_byte(reader, ref, eat_byte_safe(reader, c));
	}
	else if (read_PN_CHARS(reader, ref)) {
		r_err(reader, SERD_ERR_BAD_SYNTAX, "invalid name start character\n");
		return pop_node(reader, ref);
	}

	while ((c = peek_byte(reader))) {  // Middle: (PN_CHARS | '.')*
		if (c == '.') {
			push_byte(reader, ref, eat_byte_safe(reader, c));
		}
		else if (read_PN_CHARS(reader, ref)) {
			break;
		}
	}

	SerdNode* n = deref(reader, ref);
	if (n->buf[n->n_bytes - 1] == '.' && read_PN_CHARS(reader, ref)) {
		// Ate trailing dot, pop it from stack/node and inform caller
		--n->n_bytes;
		serd_stack_pop(&reader->stack, 1);
		*ate_dot = true;
	}

	if (fancy_syntax(reader)) {
		if (is_digit(n->buf[reader->bprefix_len + 1])) {
			if ((n->buf[reader->bprefix_len]) == 'b') {
				((char*)n->buf)[reader->bprefix_len] = 'B';  // Prevent clash
				reader->seen_genid = true;
			}
			else if (reader->seen_genid &&
				n->buf[reader->bprefix_len] == 'B') {
				r_err(reader, SERD_ERR_ID_CLASH,
					"found both `b' and `B' blank IDs, prefix required\n");
				return pop_node(reader, ref);
			}
		}
	}
	return ref;
}

static Ref
read_blankName(SerdReader* reader)
{
	eat_byte_safe(reader, '=');
	if (eat_byte_check(reader, '=') != '=') {
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "expected `='\n");
	}

	Ref  subject = 0;
	bool ate_dot = false;
	read_ws_star(reader);
	read_iri(reader, &subject, &ate_dot);
	return subject;
}

static bool
read_anon(SerdReader* reader, ReadContext ctx, bool subject, Ref* dest)
{
	const SerdStatementFlags old_flags = *ctx.flags;
	bool empty;
	eat_byte_safe(reader, '[');
	if ((empty = peek_delim(reader, ']'))) {
		*ctx.flags |= (subject) ? SERD_EMPTY_S : SERD_EMPTY_O;
	}
	else {
		*ctx.flags |= (subject) ? SERD_ANON_S_BEGIN : SERD_ANON_O_BEGIN;
		if (peek_delim(reader, '=')) {
			if (!(*dest = read_blankName(reader)) ||
				!eat_delim(reader, ';')) {
				return false;
			}
		}
	}

	if (!*dest) {
		*dest = blank_id(reader);
	}
	if (ctx.subject) {
		TRY_RET(emit_statement(reader, ctx, *dest, 0, 0));
	}

	ctx.subject = *dest;
	if (!empty) {
		*ctx.flags &= ~(unsigned)SERD_LIST_CONT;
		if (!subject) {
			*ctx.flags |= SERD_ANON_CONT;
		}
		bool ate_dot_in_list = false;
		read_predicateObjectList(reader, ctx, &ate_dot_in_list);
		if (ate_dot_in_list) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX, "`.' inside blank\n");
		}
		read_ws_star(reader);
		if (reader->end_sink) {
			reader->end_sink(reader->handle, deref(reader, *dest));
		}
		*ctx.flags = old_flags;
	}
	return (eat_byte_check(reader, ']') == ']');
}

/* If emit is true: recurses, calling statement_sink for every statement
   encountered, and leaves stack in original calling state (i.e. pops
   everything it pushes). */
static bool
read_object(SerdReader* reader, ReadContext* ctx, bool emit, bool* ate_dot)
{
	static const char* const XSD_BOOLEAN = NS_XSD "boolean";
	static const size_t      XSD_BOOLEAN_LEN = 40;

#ifndef NDEBUG
	const size_t orig_stack_size = reader->stack.size;
#endif

	bool      ret = false;
	bool      simple = (ctx->subject != 0);
	SerdNode* node = NULL;
	Ref       o = 0;
	Ref       datatype = 0;
	Ref       lang = 0;
	uint32_t  flags = 0;
	const int c = peek_byte(reader);
	if (!fancy_syntax(reader)) {
		switch (c) {
		case '"': case ':': case '<': case '_': break;
		default: return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"expected: ':', '<', or '_'\n");
		}
	}
	switch (c) {
	case EOF: case '\0': case ')':
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "expected object\n");
	case '[':
		simple = false;
		TRY_THROW(ret = read_anon(reader, *ctx, false, &o));
		break;
	case '(':
		simple = false;
		TRY_THROW(ret = read_collection(reader, *ctx, &o));
		break;
	case '_':
		TRY_THROW(ret = (o = read_BLANK_NODE_LABEL(reader, ate_dot)));
		break;
	case '<': case ':':
		TRY_THROW(ret = read_iri(reader, &o, ate_dot));
		break;
	case '+': case '-': case '.': case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7': case '8': case '9':
		TRY_THROW(ret = read_number(reader, &o, &datatype, ate_dot));
		break;
	case '\"':
	case '\'':
		TRY_THROW(ret = read_literal(reader, &o, &datatype, &lang, &flags, ate_dot));
		break;
	default:
		/* Either a boolean literal, or a qname.  Read the prefix first, and if
		   it is in fact a "true" or "false" literal, produce that instead.
		*/
		o = push_node(reader, SERD_CURIE, "", 0);
		while (!read_PN_CHARS_BASE(reader, o)) {}
		node = deref(reader, o);
		if ((node->n_bytes == 4 && !memcmp(node->buf, "true", 4)) ||
			(node->n_bytes == 5 && !memcmp(node->buf, "false", 5))) {
			node->type = SERD_LITERAL;
			datatype = push_node(
				reader, SERD_URI, XSD_BOOLEAN, XSD_BOOLEAN_LEN);
			ret = true;
		}
		else if (read_PN_PREFIX_tail(reader, o) > SERD_FAILURE) {
			ret = false;
		}
		else {
			if (!(ret = read_PrefixedName(reader, o, false, ate_dot))) {
				r_err(reader, SERD_ERR_BAD_SYNTAX, "expected prefixed name\n");
			}
		}
	}

	if (simple && o) {
		deref(reader, o)->flags = flags;
	}

	if (ret && emit && simple) {
		ret = emit_statement(reader, *ctx, o, datatype, lang);
	}
	else if (ret && !emit) {
		ctx->object = o;
		ctx->datatype = datatype;
		ctx->lang = lang;
		return true;
	}

except:
	pop_node(reader, lang);
	pop_node(reader, datatype);
	pop_node(reader, o);
#ifndef NDEBUG
	assert(reader->stack.size == orig_stack_size);
#endif
	return ret;
}

static bool
read_objectList(SerdReader* reader, ReadContext ctx, bool* ate_dot)
{
	TRY_RET(read_object(reader, &ctx, true, ate_dot));
	if (!fancy_syntax(reader) && peek_delim(reader, ',')) {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"syntax does not support abbreviation\n");
	}

	while (!*ate_dot && eat_delim(reader, ',')) {
		TRY_RET(read_object(reader, &ctx, true, ate_dot));
	}
	return true;
}

static bool
read_predicateObjectList(SerdReader* reader, ReadContext ctx, bool* ate_dot)
{
	while (read_verb(reader, &ctx.predicate) &&
		read_ws_star(reader) &&
		read_objectList(reader, ctx, ate_dot)) {
		ctx.predicate = pop_node(reader, ctx.predicate);
		if (*ate_dot) {
			return true;
		}

		bool ate_semi = false;
		int  c;
		do {
			read_ws_star(reader);
			switch (c = peek_byte(reader)) {
			case EOF: case '\0':
				return r_err(reader, SERD_ERR_BAD_SYNTAX,
					"unexpected end of file\n");
			case '.': case ']': case '}':
				return true;
			case ';':
				eat_byte_safe(reader, c);
				ate_semi = true;
			}
		} while (c == ';');

		if (!ate_semi) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX, "missing ';' or '.'\n");
		}
	}

	return pop_node(reader, ctx.predicate);
}

static bool
end_collection(SerdReader* reader, ReadContext ctx, Ref n1, Ref n2, bool ret)
{
	pop_node(reader, n2);
	pop_node(reader, n1);
	*ctx.flags &= ~(unsigned)SERD_LIST_CONT;
	return ret && (eat_byte_safe(reader, ')') == ')');
}

static bool
read_collection(SerdReader* reader, ReadContext ctx, Ref* dest)
{
	eat_byte_safe(reader, '(');
	bool end = peek_delim(reader, ')');
	*dest = end ? reader->rdf_nil : blank_id(reader);
	if (ctx.subject) {
		// subject predicate _:head
		*ctx.flags |= (end ? 0 : SERD_LIST_O_BEGIN);
		TRY_RET(emit_statement(reader, ctx, *dest, 0, 0));
		*ctx.flags |= SERD_LIST_CONT;
	}
	else {
		*ctx.flags |= (end ? 0 : SERD_LIST_S_BEGIN);
	}

	if (end) {
		return end_collection(reader, ctx, 0, 0, true);
	}

	/* The order of node allocation here is necessarily not in stack order,
	   so we create two nodes and recycle them throughout. */
	Ref n1 = push_node_padded(reader, genid_size(reader), SERD_BLANK, "", 0);
	Ref n2 = 0;
	Ref node = n1;
	Ref rest = 0;

	ctx.subject = *dest;
	while (!(end = peek_delim(reader, ')'))) {
		// _:node rdf:first object
		ctx.predicate = reader->rdf_first;
		bool ate_dot = false;
		if (!read_object(reader, &ctx, true, &ate_dot) || ate_dot) {
			return end_collection(reader, ctx, n1, n2, false);
		}

		if (!(end = peek_delim(reader, ')'))) {
			/* Give rest a new ID.  Done as late as possible to ensure it is
			   used and > IDs generated by read_object above. */
			if (!rest) {
				rest = n2 = blank_id(reader);  // First pass, push
			}
			else {
				set_blank_id(reader, rest, genid_size(reader));
			}
		}

		// _:node rdf:rest _:rest
		*ctx.flags |= SERD_LIST_CONT;
		ctx.predicate = reader->rdf_rest;
		TRY_RET(emit_statement(reader, ctx,
			(end ? reader->rdf_nil : rest), 0, 0));

		ctx.subject = rest;         // _:node = _:rest
		rest = node;         // _:rest = (old)_:node
		node = ctx.subject;  // invariant
	}

	return end_collection(reader, ctx, n1, n2, true);
}

static Ref
read_subject(SerdReader* reader, ReadContext ctx, Ref* dest, int* s_type)
{
	bool ate_dot = false;
	switch ((*s_type = peek_byte(reader))) {
	case '[':
		read_anon(reader, ctx, true, dest);
		break;
	case '(':
		read_collection(reader, ctx, dest);
		break;
	case '_':
		*dest = read_BLANK_NODE_LABEL(reader, &ate_dot);
		break;
	default:
		TRY_RET(read_iri(reader, dest, &ate_dot));
	}
	return ate_dot ? pop_node(reader, *dest) : *dest;
}

static Ref
read_labelOrSubject(SerdReader* reader)
{
	Ref  subject = 0;
	bool ate_dot = false;
	switch (peek_byte(reader)) {
	case '[':
		eat_byte_safe(reader, '[');
		read_ws_star(reader);
		TRY_RET(eat_byte_check(reader, ']'));
		return blank_id(reader);
	case '_':
		return read_BLANK_NODE_LABEL(reader, &ate_dot);
	default:
		read_iri(reader, &subject, &ate_dot);
	}
	return subject;
}

static bool
read_triples(SerdReader* reader, ReadContext ctx, bool* ate_dot)
{
	bool ret = false;
	if (ctx.subject) {
		read_ws_star(reader);
		switch (peek_byte(reader)) {
		case '.':
			*ate_dot = eat_byte_safe(reader, '.');
			return false;
		case '}':
			return false;
		}
		ret = read_predicateObjectList(reader, ctx, ate_dot);
	}
	ctx.subject = ctx.predicate = 0;
	return ret;
}

static bool
read_base(SerdReader* reader, bool sparql, bool token)
{
	if (token) {
		TRY_RET(eat_string(reader, "base", 4));
	}

	Ref uri;
	read_ws_star(reader);
	TRY_RET(uri = read_IRIREF(reader));
	if (reader->base_sink) {
		reader->base_sink(reader->handle, deref(reader, uri));
	}
	pop_node(reader, uri);

	read_ws_star(reader);
	if (!sparql) {
		return eat_byte_check(reader, '.');
	}
	else if (peek_byte(reader) == '.') {
		return r_err(reader, SERD_ERR_BAD_SYNTAX,
			"full stop after SPARQL BASE\n");
	}
	return true;
}

static bool
read_prefixID(SerdReader* reader, bool sparql, bool token)
{
	if (token) {
		TRY_RET(eat_string(reader, "prefix", 6));
	}

	read_ws_star(reader);
	bool ret = true;
	Ref  name = push_node(reader, SERD_LITERAL, "", 0);
	if (read_PN_PREFIX(reader, name) > SERD_FAILURE) {
		return pop_node(reader, name);
	}

	if (eat_byte_check(reader, ':') != ':') {
		return pop_node(reader, name);
	}

	read_ws_star(reader);
	const Ref uri = read_IRIREF(reader);
	if (!uri) {
		pop_node(reader, name);
		return false;
	}

	if (reader->prefix_sink) {
		ret = !reader->prefix_sink(reader->handle,
			deref(reader, name),
			deref(reader, uri));
	}
	pop_node(reader, uri);
	pop_node(reader, name);
	if (!sparql) {
		read_ws_star(reader);
		return eat_byte_check(reader, '.');
	}
	return ret;
}

static bool
read_directive(SerdReader* reader)
{
	const bool sparql = peek_byte(reader) != '@';
	if (!sparql) {
		eat_byte_safe(reader, '@');
		switch (peek_byte(reader)) {
		case 'B': case 'P':
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"uppercase directive\n");
		}
	}

	switch (peek_byte(reader)) {
	case 'B': case 'b': return read_base(reader, sparql, true);
	case 'P': case 'p': return read_prefixID(reader, sparql, true);
	default:
		return r_err(reader, SERD_ERR_BAD_SYNTAX, "invalid directive\n");
	}

	return true;
}

static bool
read_wrappedGraph(SerdReader* reader, ReadContext* ctx)
{
	TRY_RET(eat_byte_check(reader, '{'));
	read_ws_star(reader);
	while (peek_byte(reader) != '}') {
		bool ate_dot = false;
		int  s_type = 0;
		ctx->subject = 0;
		Ref subj = read_subject(reader, *ctx, &ctx->subject, &s_type);
		if (!subj && ctx->subject) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX, "bad subject\n");
		}
		else if (!subj) {
			return false;
		}
		else if (!read_triples(reader, *ctx, &ate_dot) && s_type != '[') {
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"missing predicate object list\n");
		}
		pop_node(reader, subj);
		read_ws_star(reader);
		if (peek_byte(reader) == '.') {
			eat_byte_safe(reader, '.');
		}
		read_ws_star(reader);
	}
	return eat_byte_check(reader, '}');
}

static int
tokcmp(SerdReader* reader, Ref ref, const char* tok, size_t n)
{
	SerdNode* node = deref(reader, ref);
	if (!node || node->n_bytes != n) {
		return -1;
	}
	return serd_strncasecmp((const char*)node->buf, tok, n);
}

bool
read_n3_statement(SerdReader* reader)
{
	SerdStatementFlags flags = 0;
	ReadContext        ctx = { 0, 0, 0, 0, 0, 0, &flags };
	Ref                subj = 0;
	bool               ate_dot = false;
	int                s_type = 0;
	bool               ret = true;
	read_ws_star(reader);
	switch (peek_byte(reader)) {
	case EOF: case '\0':
		return reader->status <= SERD_FAILURE;
	case '@':
		if (!fancy_syntax(reader)) {
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"syntax does not support directives\n");
		}
		TRY_RET(read_directive(reader));
		read_ws_star(reader);
		break;
	case '{':
		if (reader->syntax == SERD_TRIG) {
			TRY_RET(read_wrappedGraph(reader, &ctx));
			read_ws_star(reader);
		}
		else {
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"syntax does not support graphs\n");
		}
		break;
	default:
		subj = read_subject(reader, ctx, &ctx.subject, &s_type);
		if (!tokcmp(reader, ctx.subject, "base", 4)) {
			ret = read_base(reader, true, false);
		}
		else if (!tokcmp(reader, ctx.subject, "prefix", 6)) {
			ret = read_prefixID(reader, true, false);
		}
		else if (!tokcmp(reader, ctx.subject, "graph", 5)) {
			read_ws_star(reader);
			TRY_RET((ctx.graph = read_labelOrSubject(reader)));
			read_ws_star(reader);
			TRY_RET(read_wrappedGraph(reader, &ctx));
			pop_node(reader, ctx.graph);
			ctx.graph = 0;
			read_ws_star(reader);
		}
		else if (read_ws_star(reader) && peek_byte(reader) == '{') {
			if (s_type == '(' || (s_type == '[' && !*ctx.flags)) {
				return r_err(reader, SERD_ERR_BAD_SYNTAX,
					"invalid graph name\n");
			}
			ctx.graph = subj;
			ctx.subject = subj = 0;
			TRY_RET(read_wrappedGraph(reader, &ctx));
			pop_node(reader, ctx.graph);
			read_ws_star(reader);
		}
		else if (!subj) {
			ret = r_err(reader, SERD_ERR_BAD_SYNTAX, "bad subject\n");
		}
		else if (!read_triples(reader, ctx, &ate_dot)) {
			if (!(ret = (s_type == '[')) && ate_dot) {
				ret = r_err(reader, SERD_ERR_BAD_SYNTAX,
					"unexpected end of statement\n");
			}
		}
		else if (!ate_dot) {
			read_ws_star(reader);
			ret = (eat_byte_check(reader, '.') == '.');
		}
		pop_node(reader, subj);
		break;
	}
	return ret;
}

static void
skip_until(SerdReader* reader, uint8_t byte)
{
	for (int c = 0; (c = peek_byte(reader)) && c != byte;) {
		eat_byte_safe(reader, c);
	}
}

bool
read_turtleTrigDoc(SerdReader* reader)
{
	while (!reader->source.eof) {
		if (!read_n3_statement(reader)) {
			if (reader->strict) {
				return 0;
			}
			skip_until(reader, '\n');
			reader->status = SERD_SUCCESS;
		}
	}
	return reader->status <= SERD_FAILURE;
}

bool
read_nquadsDoc(SerdReader* reader)
{
	while (!reader->source.eof) {
		SerdStatementFlags flags = 0;
		ReadContext        ctx = { 0, 0, 0, 0, 0, 0, &flags };
		bool               ate_dot = false;
		int                s_type = 0;
		read_ws_star(reader);
		if (peek_byte(reader) == EOF) {
			break;
		}
		else if (peek_byte(reader) == '@') {
			return r_err(reader, SERD_ERR_BAD_SYNTAX,
				"syntax does not support directives\n");
		}

		// subject predicate object
		if (!(ctx.subject = read_subject(reader, ctx, &ctx.subject, &s_type)) ||
			!read_ws_star(reader) ||
			!(ctx.predicate = read_IRIREF(reader)) ||
			!read_ws_star(reader) ||
			!read_object(reader, &ctx, false, &ate_dot)) {
			return false;
		}

		if (!ate_dot) {  // graphLabel?
			TRY_RET(read_ws_star(reader));
			switch (peek_byte(reader)) {
			case '.':
				break;
			case '_':
				ctx.graph = read_BLANK_NODE_LABEL(reader, &ate_dot);
				break;
			default:
				if (!(ctx.graph = read_IRIREF(reader))) {
					return false;
				}
			}

			// Terminating '.'
			TRY_RET(read_ws_star(reader));
			eat_byte_check(reader, '.');
		}

		TRY_RET(emit_statement(reader, ctx, ctx.object, ctx.datatype, ctx.lang));
		pop_node(reader, ctx.graph);
		pop_node(reader, ctx.lang);
		pop_node(reader, ctx.datatype);
		pop_node(reader, ctx.object);
	}
	return reader->status <= SERD_FAILURE;
}


typedef struct {
	SerdNode name;
	SerdNode uri;
} SerdPrefix;

struct SerdEnvImpl {
	SerdPrefix* prefixes;
	size_t      n_prefixes;
	SerdNode    base_uri_node;
	SerdURI     base_uri;
};

SerdEnv*
serd_env_new(const SerdNode* base_uri)
{
	SerdEnv* env = (SerdEnv*)calloc(1, sizeof(struct SerdEnvImpl));
	if (env && base_uri) {
		serd_env_set_base_uri(env, base_uri);
	}
	return env;
}

void
serd_env_free(SerdEnv* env)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		serd_node_free(&env->prefixes[i].name);
		serd_node_free(&env->prefixes[i].uri);
	}
	free(env->prefixes);
	serd_node_free(&env->base_uri_node);
	free(env);
}

const SerdNode*
serd_env_get_base_uri(const SerdEnv* env,
	SerdURI* out)
{
	if (out) {
		*out = env->base_uri;
	}
	return &env->base_uri_node;
}

SerdStatus
serd_env_set_base_uri(SerdEnv* env,
	const SerdNode* uri)
{
	if (!env || !uri) {
		return SERD_ERR_BAD_ARG;
	}

	// Resolve base URI and create a new node and URI for it
	SerdURI  base_uri;
	SerdNode base_uri_node = serd_node_new_uri_from_node(
		uri, &env->base_uri, &base_uri);

	if (base_uri_node.buf) {
		// Replace the current base URI
		serd_node_free(&env->base_uri_node);
		env->base_uri_node = base_uri_node;
		env->base_uri = base_uri;
		return SERD_SUCCESS;
	}
	return SERD_ERR_BAD_ARG;
}

static inline SerdPrefix*
serd_env_find(const SerdEnv* env,
	const uint8_t* name,
	size_t         name_len)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		const SerdNode* const prefix_name = &env->prefixes[i].name;
		if (prefix_name->n_bytes == name_len) {
			if (!memcmp(prefix_name->buf, name, name_len)) {
				return &env->prefixes[i];
			}
		}
	}
	return NULL;
}

static void
serd_env_add(SerdEnv* env,
	const SerdNode* name,
	const SerdNode* uri)
{
	SerdPrefix* const prefix = serd_env_find(env, name->buf, name->n_bytes);
	if (prefix) {
		SerdNode old_prefix_uri = prefix->uri;
		prefix->uri = serd_node_copy(uri);
		serd_node_free(&old_prefix_uri);
	}
	else {
		env->prefixes = (SerdPrefix*)realloc(
			env->prefixes, (++env->n_prefixes) * sizeof(SerdPrefix));
		env->prefixes[env->n_prefixes - 1].name = serd_node_copy(name);
		env->prefixes[env->n_prefixes - 1].uri = serd_node_copy(uri);
	}
}

SerdStatus
serd_env_set_prefix(SerdEnv* env,
	const SerdNode* name,
	const SerdNode* uri)
{
	if (!name->buf || uri->type != SERD_URI) {
		return SERD_ERR_BAD_ARG;
	}
	else if (serd_uri_string_has_scheme(uri->buf)) {
		// Set prefix to absolute URI
		serd_env_add(env, name, uri);
	}
	else {
		// Resolve relative URI and create a new node and URI for it
		SerdURI  abs_uri;
		SerdNode abs_uri_node = serd_node_new_uri_from_node(
			uri, &env->base_uri, &abs_uri);

		// Set prefix to resolved (absolute) URI
		serd_env_add(env, name, &abs_uri_node);
		serd_node_free(&abs_uri_node);
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_env_set_prefix_from_strings(SerdEnv* env,
	const uint8_t* name,
	const uint8_t* uri)
{
	const SerdNode name_node = serd_node_from_string(SERD_LITERAL, name);
	const SerdNode uri_node = serd_node_from_string(SERD_URI, uri);

	return serd_env_set_prefix(env, &name_node, &uri_node);
}

bool
serd_env_qualify(const SerdEnv* env,
	const SerdNode* uri,
	SerdNode* prefix,
	SerdChunk* suffix)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		const SerdNode* const prefix_uri = &env->prefixes[i].uri;
		if (uri->n_bytes >= prefix_uri->n_bytes) {
			if (!strncmp((const char*)uri->buf,
				(const char*)prefix_uri->buf,
				prefix_uri->n_bytes)) {
				*prefix = env->prefixes[i].name;
				suffix->buf = uri->buf + prefix_uri->n_bytes;
				suffix->len = uri->n_bytes - prefix_uri->n_bytes;
				return true;
			}
		}
	}
	return false;
}

SerdStatus
serd_env_expand(const SerdEnv* env,
	const SerdNode* curie,
	SerdChunk* uri_prefix,
	SerdChunk* uri_suffix)
{
	const uint8_t* const colon = (const uint8_t*)memchr(
		curie->buf, ':', curie->n_bytes + 1);
	if (curie->type != SERD_CURIE || !colon) {
		return SERD_ERR_BAD_ARG;
	}

	const size_t            name_len = (size_t)(colon - curie->buf);
	const SerdPrefix* const prefix = serd_env_find(env, curie->buf, name_len);
	if (prefix) {
		uri_prefix->buf = prefix->uri.buf;
		uri_prefix->len = prefix->uri.n_bytes;
		uri_suffix->buf = colon + 1;
		uri_suffix->len = curie->n_bytes - name_len - 1;
		return SERD_SUCCESS;
	}
	return SERD_ERR_BAD_CURIE;
}

SerdNode
serd_env_expand_node(const SerdEnv* env,
	const SerdNode* node)
{
	switch (node->type) {
	case SERD_CURIE: {
		SerdChunk prefix;
		SerdChunk suffix;
		if (serd_env_expand(env, node, &prefix, &suffix)) {
			return SERD_NODE_NULL;
		}
		const size_t len = prefix.len + suffix.len;
		uint8_t* buf = (uint8_t*)malloc(len + 1);
		SerdNode     ret = { buf, len, 0, 0, SERD_URI };
		snprintf((char*)buf, len + 1, "%s%s", prefix.buf, suffix.buf);
		ret.n_chars = serd_strlen(buf, NULL, NULL);
		return ret;
	}
	case SERD_URI: {
		SerdURI ignored;
		return serd_node_new_uri_from_node(node, &env->base_uri, &ignored);
	}
	default:
		return SERD_NODE_NULL;
	}
}

void
serd_env_foreach(const SerdEnv* env,
	SerdPrefixSink func,
	void* handle)
{
	for (size_t i = 0; i < env->n_prefixes; ++i) {
		func(handle, &env->prefixes[i].name, &env->prefixes[i].uri);
	}
}

SerdStatus
serd_byte_source_page(SerdByteSource* source)
{
	source->read_head = 0;
	const size_t n_read = source->read_func(
		source->file_buf, 1, source->page_size, source->stream);
	if (n_read == 0) {
		source->file_buf[0] = '\0';
		source->eof = true;
		return (source->error_func(source->stream)
			? SERD_ERR_UNKNOWN : SERD_FAILURE);
	}
	else if (n_read < source->page_size) {
		source->file_buf[n_read] = '\0';
		source->buf_size = n_read;
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_open_source(SerdByteSource* source,
	SerdSource          read_func,
	SerdStreamErrorFunc error_func,
	void* stream,
	const uint8_t* name,
	size_t              page_size)
{
	const Cursor cur = { name, 1, 1 };

	memset(source, '\0', sizeof(*source));
	source->stream = stream;
	source->from_stream = true;
	source->page_size = page_size;
	source->buf_size = page_size;
	source->cur = cur;
	source->error_func = error_func;
	source->read_func = read_func;

	if (page_size > 1) {
		source->file_buf = (uint8_t*)serd_bufalloc(page_size);
		source->read_buf = source->file_buf;
		memset(source->file_buf, '\0', page_size);
	}
	else {
		source->read_buf = &source->read_byte;
	}

	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_prepare(SerdByteSource* source)
{
	source->prepared = true;
	if (source->from_stream) {
		if (source->page_size > 1) {
			return serd_byte_source_page(source);
		}
		else if (source->from_stream) {
			return serd_byte_source_advance(source);
		}
	}
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_open_string(SerdByteSource* source, const uint8_t* utf8)
{
	const Cursor cur = { (const uint8_t*)"(string)", 1, 1 };

	memset(source, '\0', sizeof(*source));
	source->cur = cur;
	source->read_buf = utf8;
	return SERD_SUCCESS;
}

SerdStatus
serd_byte_source_close(SerdByteSource* source)
{
	if (source->page_size > 1) {
		free(source->file_buf);
	}
	memset(source, '\0', sizeof(*source));
	return SERD_SUCCESS;
}
