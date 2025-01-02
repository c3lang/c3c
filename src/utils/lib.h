#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "common.h"
#include "setjmp.h"

#if PLATFORM_WINDOWS
#include "direct.h"
#include "intrin.h"
#endif

#if FETCH_AVAILABLE
const char *download_file(const char *url, const char *resource, const char *file_path);
#endif

#define ELEMENTLEN(x) (sizeof(x) / sizeof(x[0]))
extern const char *compiler_exe_name;

typedef struct StringSlice_
{
	const char *ptr;
	size_t len;
} StringSlice;

typedef struct
{
	int major;
	int minor;
} Version;

typedef struct
{
	Version macos_deploy_target;
	Version macos_min_deploy_target;
} MacSDK;

typedef struct {
	char* windows_sdk_path;
	char* vs_library_path;
} WindowsSDK;

#define MAX_STRING_BUFFER 0x10000
#define COMPILER_SUCCESS_EXIT -1000
NORETURN void exit_compiler(int exit_value);
extern jmp_buf on_err_jump;

extern bool debug_log;

extern uintptr_t arena_zero;
struct ScratchBuf { char str[MAX_STRING_BUFFER]; uint32_t len; };
extern struct ScratchBuf scratch_buffer;


typedef struct Task_
{
	void (*task)(void *arg);
	void *arg;
} Task;

uint16_t *win_utf8to16(const char *name);
char *win_utf16to8(const uint16_t *name);
// Use as if it was mkdir(..., 0755) == 0
bool dir_make(const char *path);
bool dir_make_recursive(char *path);
// Use as if it was chdir(...) == 0
bool dir_change(const char *path);
const char *filename(const char *path);
bool file_namesplit(const char *path, char** filename_ptr, char** directory_ptr);
const char* file_expand_path(const char* path);
const char* find_lib_dir(void);
const char *find_rel_exe_dir(const char *dir);

void file_copy_file(const char *src_path, const char *dst_path, bool overwrite);
void file_delete_all_files_in_dir_with_suffix(const char *dir, const char *suffix);
bool file_delete_file(const char *path);
bool file_is_dir(const char *file);
bool file_exists(const char *path);
FILE *file_open_read(const char *path);
bool file_touch(const char *path);
char *file_read_binary(const char *path, size_t *size);
char *file_read_all(const char *path, size_t *return_size);
void file_get_dir_and_filename_from_full(const char *full_path, char **filename, char **dir_path);
void file_find_top_dir();
bool file_has_suffix_in_list(const char *file_name, int name_len, const char **suffix_list, int suffix_count);
void file_add_wildcard_files(const char ***files, const char *path, bool recursive, const char **suffix_list, int suffix_count);
const char *file_append_path(const char *path, const char *name);
const char *file_append_path_temp(const char *path, const char *name);

const char **target_expand_source_names(const char *base_dir, const char** dirs, const char **suffix_list, const char ***object_list_ref, int suffix_count, bool error_on_mismatch);

const char *execute_cmd(const char *cmd, bool ignore_failure, const char *stdin_string);

bool execute_cmd_failable(const char *cmd, const char **result, const char *stdin_string);
void *cmalloc(size_t size);
void *ccalloc(size_t size, size_t elements);
void memory_init(size_t max_mem);
void memory_release();

#define ptrid(ptr_) ((((uintptr_t)(ptr_)) - arena_zero) / 16)
#define idptr(id_) ((void*)(((uintptr_t)id_) * 16 + arena_zero))
void *calloc_arena(size_t mem);
char *calloc_string(size_t len);
#ifdef NDEBUG
#define malloc_string calloc_string
#define malloc_arena calloc_arena
#else
static inline void *malloc_string(size_t len)
{
	void *data = calloc_string(len);
	memset(data, 0xaa, len);
	return data;
}
static inline void *malloc_arena(size_t len)
{
	void *data = calloc_arena(len);
	memset(data, 0xaa, len);
	return data;
}
#endif
void free_arena(void);
void print_arena_status(void);
void run_arena_allocator_tests(void);
void taskqueue_run(int threads, Task **task_list);
int cpus(void);
const char *date_get(void);
const char *time_get(void);

const char *str_remove_suffix(const char *name, const char *suffix);
bool str_has_suffix(const char *name, const char *suffix);
char *str_trim(char *str);
const char *str_trim_start(const char *str);
void str_trim_end(char *str);
char *str_cat(const char *a, const char *b);
// Search a list of strings and return the matching element or -1 if none found.
int str_findlist(const char *value, unsigned count, const char** elements);
// Sprintf style, saved to an arena allocated string
char *str_printf(const char *var, ...) __printflike(1, 2);
char *str_vprintf(const char *var, va_list list);
void str_ellide_in_place(char *string, size_t max_size_shown);
bool str_is_valid_lowercase_name(const char *string);
bool str_is_valid_constant(const char *string);
const char *str_unescape(char *string);
bool str_is_identifier(const char *string);
bool str_eq(const char *str1, const char *str2);
bool str_is_type(const char *string);
bool str_is_integer(const char *string);
bool str_has_no_uppercase(const char *string);
bool str_is_valid_module_name(const char *name);
char *str_copy(const char *start, size_t str_len);

StringSlice slice_next_token(StringSlice *slice, char separator);
static inline bool slice_strcmp(StringSlice slice, const char *other);
static inline StringSlice slice_from_string(const char *data);
void slice_trim(StringSlice *slice);

void scratch_buffer_clear(void);
void scratch_buffer_append(const char *string);
void scratch_buffer_append_len(const char *string, size_t len);
void scratch_buffer_append_char(char c);
void scratch_buffer_append_in_quote(const char *string);
void scratch_buffer_append_char_repeat(char c, size_t count);
void scratch_buffer_append_remove_space(const char *start, int len);
void scratch_buffer_append_signed_int(int64_t i);
void scratch_buffer_append_double(double d);
void scratch_buffer_append_shell_escaped(const char *string);
void scratch_buffer_append_cmd_argument(const char *string);
UNUSED void scratch_buffer_append_unsigned_int(uint64_t i);
void scratch_buffer_printf(const char *format, ...);
char *scratch_buffer_to_string(void);
char *scratch_buffer_copy(void);

static inline bool is_power_of_two(uint64_t x);
static inline uint32_t next_highest_power_of_2(uint32_t v);

static inline bool char_is_lower(char c);
static inline bool char_is_lower_(char c);
static inline bool char_is_upper(char c);
static inline bool char_is_oct(char c);
static inline bool char_is_oct_or_(char c);
static inline bool char_is_binary(char c);
static inline bool char_is_binary_or_(char c);
static inline bool char_is_digit_or_(char c);
static inline bool char_is_digit(char c);
static inline bool char_is_hex(char c);
static inline bool char_is_hex_or_(char c);
static inline bool char_is_base64(char c);
static inline bool char_is_letter(char c);
static inline bool char_is_letter_(char c);
static inline bool char_is_alphanum_(char c);
static inline bool char_is_lower_alphanum_(char c);
static inline bool char_is_whitespace(char c);
static inline signed char char_is_valid_escape(char c);
// Hex to nibble, -1 if invalid
static inline int char_hex_to_nibble(char c);
INLINE char char_nibble_to_hex(int c);

static inline uint32_t fnv1a(const char *key, uint32_t len);

INLINE uint32_t vec_size(const void *vec);
static inline void vec_resize(void *vec, uint32_t new_size);
static inline void vec_pop(void *vec);
static inline void vec_erase_at(void *vec, unsigned i);

#define NUMBER_CHAR_CASE '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9'
#define UPPER_CHAR_CASE 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': \
	case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': \
	case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z'
#define LOWER_CHAR_CASE 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': \
	case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': \
	case 'u': case 'v': case 'w': case 'x': case 'y': case 'z'
#define HEX_CHAR_CASE  'a': case 'b': case 'c': case 'd': case 'e': case 'f': \
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F'

#if MEM_PRINT
#define MALLOC(mem) (printf("Alloc at %s %zu\n", __FUNCTION__, (size_t)(mem)), malloc_arena(mem))
#define MALLOCS(type) (printf("calloc at %s %zu\n", __FUNCTION__, sizeof(type)), malloc_arena(sizeof(type)))
#define CALLOC(mem) (printf("calloc at %s %zu\n", __FUNCTION__, (size_t)(mem)), calloc_arena(mem))
#define CALLOCS(type) (printf("calloc at %s %zu\n", __FUNCTION__, sizeof(type)), calloc_arena(sizeof(type)))
#elif NO_ARENA
#define MALLOC(mem) malloc(mem)
#define MALLOCS(type) malloc(sizeof(type))
#define CALLOC(mem) calloc(16 * (((mem) + 15) / 16), 16)
#define CALLOCS(type) calloc(1, sizeof(type))
#else
#define MALLOC(mem) malloc_arena(mem)
#define MALLOCS(type) malloc_arena(sizeof(type))
#define CALLOC(mem) calloc_arena(mem)
#define CALLOCS(type) calloc_arena(sizeof(type))
#endif


#define FNV1_PRIME 0x01000193u
#define FNV1_SEED 0x811C9DC5u
#define FNV1a(c, seed) ((uint32_t)((((unsigned)(c)) ^ (seed)) * FNV1_PRIME))

static inline uint32_t fnv1a(const char *key, uint32_t len)
{
	uint32_t hash = FNV1_SEED;
	for (uint32_t i = 0; i < len; i++)
	{
		hash = FNV1a(key[i], hash);
	}
	return hash;
}

typedef struct
{
	uint32_t size;
	uint32_t capacity;
	char data[];
} VHeader_;

static inline VHeader_* vec_new_(size_t element_size, size_t capacity)
{
	ASSERT0(capacity < UINT32_MAX);
	ASSERT0(element_size < UINT32_MAX / 100);
	VHeader_ *header = CALLOC(element_size * capacity + sizeof(VHeader_));
	header->capacity = (uint32_t)capacity;
	return header;
}


static inline void vec_resize(void *vec, uint32_t new_size)
{
	if (!vec) return;
	VHeader_ *header = vec;
	header[-1].size = new_size;
}


static inline void vec_pop(void *vec)
{
	ASSERT0(vec);
	ASSERT0(vec_size(vec) > 0);
	VHeader_ *header = vec;
	header[-1].size--;
}

static inline void vec_erase_front(void  *vec, unsigned to_erase)
{
	if (!to_erase) return;
	ASSERT0(vec);
	unsigned size = vec_size(vec);
	ASSERT0(size >= to_erase);
	void **vecptr = (void**)vec;
	for (int i = to_erase; i < size; i++)
	{
		vecptr[i - to_erase] = vecptr[i];
	}
	VHeader_ *header = vec;
	header[-1].size -= to_erase;
}

static inline void vec_erase_at(void *vec, unsigned i)
{
	ASSERT0(vec);
	unsigned size = vec_size(vec);
	ASSERT0(size > i);
	void **vecptr = (void**)vec;
	for (int j = i + 1; j < size; j++)
	{
		vecptr[j - 1] = vecptr[j];
	}
	VHeader_ *header = vec;
	header[-1].size--;
}

static inline void* expand_(void *vec, size_t element_size)
{
	VHeader_ *header;
	if (!vec)
	{
		header = vec_new_(element_size, 8);
	}
	else
	{
		header = ((VHeader_ *)vec) - 1;
	}
	if (header->size == header->capacity)
	{
		VHeader_ *new_array = vec_new_(element_size, header->capacity << 1U);
#if IS_GCC
		// I've yet to figure out why GCC insists that this is trying to copy
		// 8 bytes over a size zero array.
		// We use volatile to deoptimize on GCC
		volatile size_t copy_size = element_size * header->capacity + sizeof(VHeader_);
#else
		size_t copy_size = element_size * header->capacity + sizeof(VHeader_);
#endif
		memcpy(new_array, header, copy_size);
		header = new_array;
		new_array->capacity = header->capacity << 1U;
	}
	header->size++;
	return &(header[1]);
}


#define CONCAT_INNER(a, b) a ## b
#define CONCAT(a, b) CONCAT_INNER(a, b)

#define FOREACH(type__, name__, vec__) \
type__* CONCAT(foreach_vec_, __LINE__) = (vec__); type__* CONCAT(foreach_vecend_, __LINE__) = CONCAT(foreach_vec_, __LINE__) ? CONCAT(foreach_vec_, __LINE__) + vec_size(CONCAT(foreach_vec_, __LINE__)) : NULL; \
type__* CONCAT(foreach_it_, __LINE__) = CONCAT(foreach_vec_, __LINE__); \
for (type__ name__ ; CONCAT(foreach_it_, __LINE__) < CONCAT(foreach_vecend_, __LINE__) ? (name__ = *CONCAT(foreach_it_, __LINE__), true) : false; CONCAT(foreach_it_, __LINE__)++)

#define FOREACH_IDX(idx__, type__, name__, vec__) \
type__* CONCAT(foreach_vec_, __LINE__) = (vec__); uint32_t CONCAT(foreach_vecsize_, __LINE__) = vec_size(CONCAT(foreach_vec_, __LINE__)); \
uint32_t idx__ = 0; \
for (type__ name__ ; idx__ < CONCAT(foreach_vecsize_, __LINE__) ? (name__ = CONCAT(foreach_vec_, __LINE__)[idx__], true) : false; idx__++)

#define VECNEW(_type, _capacity) ((_type *)(vec_new_(sizeof(_type), _capacity) + 1))
#define vec_add(vec_, value_) do { \
	void *__temp = expand_((vec_), sizeof(*(vec_))); \
	(vec_) = __temp;           \
	(vec_)[vec_size(vec_) - 1] = value_; \
 } while (0)

#define vec_insert_first(vec_, value_) do { \
 void *__temp = expand_((vec_), sizeof(*(vec_))); \
 (vec_) = __temp;                           \
 unsigned __xsize = vec_size(vec_); \
 for (unsigned __x = __xsize - 1; __x > 0; __x--) (vec_)[__x] = (vec_)[__x - 1]; \
 (vec_)[0] = value_;       \
 } while (0)

#define vec_insert_at(vec_, _at, value_) do { \
 void *__temp = expand_((vec_), sizeof(*(vec_))); \
 (vec_) = __temp;                           \
 unsigned __xsize = vec_size(vec_); \
 for (unsigned __x = __xsize - 1; __x > _at; __x--) (vec_)[__x] = (vec_)[__x - 1]; \
 (vec_)[_at] = value_;       \
 } while (0)

#if IS_GCC || IS_CLANG
#define VECLAST(_vec) ({ unsigned _size = vec_size(_vec); _size ? (_vec)[_size - 1] : NULL; })
#else
#define VECLAST(_vec) (vec_size(_vec) ? (_vec)[vec_size(_vec) - 1] : NULL)
#endif
#define vectail(_vec) (_vec)[vec_size(_vec) - 1]

#if IS_GCC || IS_CLANG

#define MAX(_a, _b) ({ \
  __auto_type __a__ = (_a); \
  __auto_type __b__ = (_b); \
  __a__ > __b__ ? __a__ : __b__; })

#define MIN(_a, _b) ({ \
  __auto_type __a__ = (_a); \
  __auto_type __b__ = (_b); \
  __a__ < __b__ ? __a__ : __b__; })

#else
#define MAX(a_, b_) ((a_) > (b_) ? (a_) : (b_))
#define MIN(a_, b_) ((a_) < (b_) ? (a_) : (b_))
#endif
// Windows-specific code


#if PLATFORM_WINDOWS


char *realpath(const char *path, char *resolved_path);

#define mkdir(name, unused) _mkdir(name)

#endif

static inline bool slice_strcmp(StringSlice slice, const char *other)
{
	if (strlen(other) != slice.len) return false;
	return strncmp(slice.ptr, other, slice.len) == 0;
}

static inline StringSlice slice_from_string(const char *data)
{
	return (StringSlice) { data, strlen(data) };
}

INLINE uint32_t vec_size(const void *vec)
{
	if (!vec) return 0;
	const VHeader_ *header = vec;
	return header[-1].size;
}

static inline bool is_power_of_two(uint64_t x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

static int clz(uint64_t num)
{
#if IS_CLANG || IS_GCC
	return (int)__builtin_ctzll(num);
#else
	unsigned long index;
	_BitScanReverse64(&index, (__int64)num);
	return (int)index;
#endif
}

static inline unsigned char power_of_2(uint64_t pot_value)
{
	return 64 - clz(pot_value);
}

static inline uint32_t next_highest_power_of_2(uint32_t v)
{
	v--;
	v |= v >> 1U;
	v |= v >> 2U;
	v |= v >> 4U;
	v |= v >> 8U;
	v |= v >> 16U;
	v++;
	return v;
}

static inline bool char_is_lower(char c)
{
	return c >= 'a' && c <= 'z';
}

static inline bool char_is_lower_(char c)
{
	return c == '_' || (c >= 'a' && c <= 'z');
}

static inline bool char_is_upper(char c)
{
	return c >= 'A' && c <= 'Z';
}

static inline bool char_is_oct(char c)
{
	return c >= '0' && c <= '7';
}

static inline bool char_is_oct_or_(char c)
{
	return c == '_' || (c >= '0' && c <= '7');
}

static inline bool char_is_binary(char c)
{
	return c  == '0' || c == '1';
}
static inline bool char_is_binary_or_(char c)
{
	return c == '0' || c == '1' || c == '_';
}

static inline bool char_is_digit_or_(char c)
{
	return c == '_' || (c >= '0' && c <= '9');
}
static inline bool char_is_digit(char c)
{
	return c >= '0' && c <= '9';
}

static char hex_conv[256] = {
		['0'] = 1,
		['1'] = 2,
		['2'] = 3,
		['3'] = 4,
		['4'] = 5,
		['5'] = 6,
		['6'] = 7,
		['7'] = 8,
		['8'] = 9,
		['9'] = 10,
		['A'] = 11,
		['B'] = 12,
		['C'] = 13,
		['D'] = 14,
		['E'] = 15,
		['F'] = 16,
		['a'] = 11,
		['b'] = 12,
		['c'] = 13,
		['d'] = 14,
		['e'] = 15,
		['f'] = 16,
};

static inline int char_hex_to_nibble(char c)
{
	return hex_conv[(unsigned)c] - 1;
}

static inline bool char_is_hex_or_(char c)
{
	switch (c)
	{
		case NUMBER_CHAR_CASE:
		case HEX_CHAR_CASE:
		case '_':
			return true;
		default:
			return false;
	}
}

static inline signed char char_is_valid_escape(char c)
{
	switch (c)
	{
		case 'a':
			return '\a';
		case 'b':
			return '\b';
		case 'e':
			return 0x1B;
		case 'f':
			return '\f';
		case 'n':
			return '\n';
		case 'r':
			return '\r';
		case 't':
			return '\t';
		case 'v':
			return '\v';
		case 'x':
			return 'x';
		case 'u':
			return 'u';
		case 'U':
			return 'U';
		case '\'':
			return '\'';
		case '"':
			return '"';
		case '\\':
			return '\\';
		case '0':
			return '\0';
		default:
			return -1;
	}
}

static inline bool char_is_base64(char c)
{
	return (c >= 'A' && c <= 'Z')
		   || (c >= 'a' && c <= 'z')
		   || (c >= '0' && c <= '9')
		   || c == '+' || c == '/';
}

static inline bool char_is_hex(char c)
{
	return hex_conv[(unsigned char)c] != 0;
}

INLINE char char_nibble_to_hex(int c)
{
	static const char *conv = "0123456789ABCDEF";
	return conv[c];
}

INLINE bool is_space(char c)
{
	return c == ' ' || c == '\t';
}

static inline bool char_is_whitespace(char c)
{
	switch (c)
	{
		case ' ':
		case '\t':
		case '\n':
			return true;
		case '\r':
			UNREACHABLE
		default:
			return false;
	}
}

static inline bool char_is_alphanum_(char c)
{
	switch (c)
	{
		case LOWER_CHAR_CASE:
		case UPPER_CHAR_CASE:
		case NUMBER_CHAR_CASE:
		case '_':
			return true;
		default:
			return false;
	}
}

static inline bool char_is_lower_alphanum_(char c)
{
	switch (c)
	{
		case LOWER_CHAR_CASE:
		case NUMBER_CHAR_CASE:
		case '_':
			return true;
		default:
			return false;
	}
}

static inline bool char_is_upper_alphanum_(char c)
{
    switch (c)
    {
        case UPPER_CHAR_CASE:
        case NUMBER_CHAR_CASE:
        case '_':
            return true;
        default:
            return false;
    }
}

static inline bool char_is_letter(char c)
{
	switch (c)
	{
		case LOWER_CHAR_CASE:
		case UPPER_CHAR_CASE:
			return true;
		default:
			return false;
	}
}

static inline bool char_is_letter_(char c)
{
	switch (c)
	{
		case LOWER_CHAR_CASE:
		case UPPER_CHAR_CASE:
		case '_':
			return true;
		default:
			return false;
	}
}

#define ZIP_MAX_NAME 512

typedef struct
{
	char name[ZIP_MAX_NAME];
	size_t offset;
	size_t uncompressed_size;
	size_t compressed_size;
	uint32_t file_crc32;
	int compression_method;
} ZipFile;

typedef struct
{
	long offset;
	int files;
	int current_file;
	FILE *file;
} ZipDirIterator;

const char *zip_dir_iterator(FILE *zip, ZipDirIterator *iterator);
const char *zip_dir_iterator_next(ZipDirIterator *iterator, ZipFile *file);
const char *zip_file_read(FILE *zip, ZipFile *file, void **buffer_ptr);
const char *zip_file_write(FILE *zip, ZipFile *file, const char *dir, bool overwrite);
