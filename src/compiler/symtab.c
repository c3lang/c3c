// Copyright (c) 2019-2023 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"



typedef struct SymtabEntry_
{
	struct SymtabEntry_* next;
	const char *value;
	uint16_t key_len;
	TokenType type : 16;
	uint32_t hash;
	uint32_t index;
	const char* symbol;
} SymtabEntry;

typedef struct
{
	SymtabEntry **bucket;
	size_t bucket_max;
	size_t bucket_mask;
} SymTab;


typedef struct _Entry
{
	const char *key;
	uint32_t key_len;
	uint32_t hash;
	void *value;
} Entry;


static SymTab symtab;

const char *attribute_list[NUMBER_OF_ATTRIBUTES];
const char *builtin_list[NUMBER_OF_BUILTINS];
const char *builtin_defines[NUMBER_OF_BUILTIN_DEFINES];
const char *type_property_list[NUMBER_OF_TYPE_PROPERTIES];
const char *kw_argc;
const char *kw_argv;
const char *kw_at_checked;
const char *kw_at_deprecated;
const char *kw_at_ensure;
const char *kw_at_param;
const char *kw_at_pure;
const char *kw_at_require;
const char *kw_at_return;
const char *kw_check_assign;
const char *kw_deprecated;
const char *kw_finalize;
const char *kw_in;
const char *kw_incr;
const char *kw_initialize;
const char *kw_inout;
const char *kw_kind;
const char *kw_len;
const char *kw_libc;
const char *kw_main;
const char *kw_mainstub;
const char *kw_nameof;
const char *kw_noinline;
const char *kw_offsetof;
const char *kw_ordinal;
const char *kw_out;
const char *kw_ptr;
const char *kw_pure;
const char *kw_return;
const char *kw_self;
const char *kw_std;
const char *kw_std__core;
const char *kw_std__core__types;
const char *kw_std__io;
const char *kw_type;
const char *kw_typekind;
const char *kw_winmain;
const char *kw_wmain;
const char *kw_FILE_NOT_FOUND;
const char *kw_IoError;

void symtab_destroy()
{
	free(symtab.bucket);
}



void symtab_init(uint32_t capacity)
{
	if (capacity < 0x100) error_exit("Too small symtab size.");
	capacity = next_highest_power_of_2(capacity);
	symtab.bucket_max = capacity;
	symtab.bucket_mask = capacity - 1;

	size_t size = capacity * sizeof(SymtabEntry*);
	symtab.bucket = malloc(size);
	// Touch all pages to improve perf(!)
	memset(symtab.bucket, 0, size);

	// Add keywords.
	for (TokenType i = TOKEN_FIRST_KEYWORD; i <= TOKEN_LAST_KEYWORD; i++)
	{
		TokenType type = i;
		const char* name = token_type_to_string(type);
		uint32_t len = (uint32_t)strlen(name);
		const char* interned = symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type);
		switch (type)
		{
			case TOKEN_RETURN:
				kw_return = interned;
				break;
			default:
				break;
		}
		assert(type == i);
		assert(symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type) == interned);
	}

	// Init some constant idents
#define KW_DEF(x) symtab_add(x, sizeof(x) - 1, fnv1a(x, sizeof(x) - 1), &type)
	TokenType type = TOKEN_CONST_IDENT;
	builtin_defines[BUILTIN_DEF_DATE] = KW_DEF("DATE");
	builtin_defines[BUILTIN_DEF_FILE] = KW_DEF("FILE");
	builtin_defines[BUILTIN_DEF_FILEPATH] = KW_DEF("FILEPATH");
	builtin_defines[BUILTIN_DEF_FUNCTION] = KW_DEF("FUNCTION");
	builtin_defines[BUILTIN_DEF_FUNC] = KW_DEF("FUNC");
	builtin_defines[BUILTIN_DEF_LINE] = KW_DEF("LINE");
	builtin_defines[BUILTIN_DEF_LINE_RAW] = KW_DEF("LINE_RAW");
	builtin_defines[BUILTIN_DEF_MODULE] = KW_DEF("MODULE");
	builtin_defines[BUILTIN_DEF_TIME] = KW_DEF("TIME");
	builtin_defines[BUILTIN_DEF_BENCHMARK_NAMES] = KW_DEF("BENCHMARK_NAMES");
	builtin_defines[BUILTIN_DEF_BENCHMARK_FNS] = KW_DEF("BENCHMARK_FNS");
	builtin_defines[BUILTIN_DEF_TEST_NAMES] = KW_DEF("TEST_NAMES");
	builtin_defines[BUILTIN_DEF_TEST_FNS] = KW_DEF("TEST_FNS");
	kw_FILE_NOT_FOUND = KW_DEF("FILE_NOT_FOUND");

	type = TOKEN_TYPE_IDENT;
	kw_typekind = KW_DEF("TypeKind");
	kw_IoError = KW_DEF("IoError");

	type = TOKEN_IDENT;
	kw_argc = KW_DEF("_$argc");
	kw_argv = KW_DEF("_$argv");
	kw_check_assign = KW_DEF("check_assign");
	kw_deprecated = KW_DEF("deprecated");
	kw_finalize = KW_DEF("finalize");
	kw_in = KW_DEF("in");
	kw_initialize = KW_DEF("initialize");
	kw_incr = KW_DEF("incr");
	kw_inout = KW_DEF("inout");
	kw_libc = KW_DEF("libc");
	kw_mainstub = KW_DEF("_$main");
	kw_main = KW_DEF("main");
	kw_nameof = KW_DEF("nameof");
	kw_noinline = KW_DEF("noinline");
	kw_offsetof = KW_DEF("offsetof");
	kw_ordinal = KW_DEF("ordinal");
	kw_out = KW_DEF("out");
	kw_ptr = KW_DEF("ptr");
	kw_pure = KW_DEF("pure");
	KW_DEF("returns");
	kw_self = KW_DEF("self");
	kw_std = KW_DEF("std");
	kw_std__core = KW_DEF("std::core");
	kw_std__core__types = KW_DEF("std::core::types");
	kw_std__io = KW_DEF("std::io");
	kw_type = KW_DEF("type");
	kw_winmain = KW_DEF("wWinMain");
	kw_wmain = KW_DEF("wmain");

	type_property_list[TYPE_PROPERTY_MAX] = builtin_list[BUILTIN_MAX] = KW_DEF("max");
	type_property_list[TYPE_PROPERTY_MIN] = builtin_list[BUILTIN_MIN] = KW_DEF("min");

	type_property_list[TYPE_PROPERTY_LEN] = kw_len = KW_DEF("len");

	type_property_list[TYPE_PROPERTY_ALIGNOF] = KW_DEF("alignof");
	type_property_list[TYPE_PROPERTY_ASSOCIATED] = KW_DEF("associated");
	type_property_list[TYPE_PROPERTY_ELEMENTS] = KW_DEF("elements");
	type_property_list[TYPE_PROPERTY_EXTNAMEOF] = KW_DEF("extnameof");
	type_property_list[TYPE_PROPERTY_INF] = KW_DEF("inf");
	type_property_list[TYPE_PROPERTY_INNER] = KW_DEF("inner");
	type_property_list[TYPE_PROPERTY_IS_EQ] = KW_DEF("is_eq");
	type_property_list[TYPE_PROPERTY_IS_ORDERED] = KW_DEF("is_ordered");
	type_property_list[TYPE_PROPERTY_KINDOF] = KW_DEF("kindof");
	type_property_list[TYPE_PROPERTY_MEMBERSOF] = KW_DEF("membersof");
	type_property_list[TYPE_PROPERTY_NAMEOF] = KW_DEF("nameof");
	type_property_list[TYPE_PROPERTY_NAMES] = KW_DEF("names");
	type_property_list[TYPE_PROPERTY_NAN] = KW_DEF("nan");
	type_property_list[TYPE_PROPERTY_PARAMS] = KW_DEF("params");
	type_property_list[TYPE_PROPERTY_PARENTOF] = KW_DEF("parentof");
	type_property_list[TYPE_PROPERTY_QNAMEOF] = KW_DEF("qnameof");
	type_property_list[TYPE_PROPERTY_RETURNS] = KW_DEF("returns");
	type_property_list[TYPE_PROPERTY_SIZEOF] = KW_DEF("sizeof");
	type_property_list[TYPE_PROPERTY_VALUES] = KW_DEF("values");

	builtin_list[BUILTIN_ABS] = KW_DEF("abs");
	builtin_list[BUILTIN_ANY_MAKE] = KW_DEF("any_make");
	builtin_list[BUILTIN_ATOMIC_LOAD] = KW_DEF("atomic_load");
	builtin_list[BUILTIN_ATOMIC_STORE] = KW_DEF("atomic_store");
	builtin_list[BUILTIN_ATOMIC_FETCH_ADD] = KW_DEF("atomic_fetch_add");
	builtin_list[BUILTIN_ATOMIC_FETCH_EXCHANGE] = KW_DEF("atomic_fetch_exchange");
	builtin_list[BUILTIN_ATOMIC_FETCH_SUB] = KW_DEF("atomic_fetch_sub");
	builtin_list[BUILTIN_ATOMIC_FETCH_MAX] = KW_DEF("atomic_fetch_max");
	builtin_list[BUILTIN_ATOMIC_FETCH_MIN] = KW_DEF("atomic_fetch_min");
	builtin_list[BUILTIN_ATOMIC_FETCH_AND] = KW_DEF("atomic_fetch_and");
	builtin_list[BUILTIN_ATOMIC_FETCH_NAND] = KW_DEF("atomic_fetch_nand");
	builtin_list[BUILTIN_ATOMIC_FETCH_OR] = KW_DEF("atomic_fetch_or");
	builtin_list[BUILTIN_ATOMIC_FETCH_XOR] = KW_DEF("atomic_fetch_xor");
	builtin_list[BUILTIN_ATOMIC_FETCH_INC_WRAP] = KW_DEF("atomic_fetch_inc_wrap");
	builtin_list[BUILTIN_ATOMIC_FETCH_DEC_WRAP] = KW_DEF("atomic_fetch_dec_wrap");
	builtin_list[BUILTIN_BITREVERSE] = KW_DEF("bitreverse");
	builtin_list[BUILTIN_BSWAP] = KW_DEF("bswap");
	builtin_list[BUILTIN_CEIL] = KW_DEF("ceil");
	builtin_list[BUILTIN_COMPARE_EXCHANGE] = KW_DEF(("compare_exchange"));
	builtin_list[BUILTIN_COPYSIGN] = KW_DEF("copysign");
	builtin_list[BUILTIN_COS] = KW_DEF("cos");
	builtin_list[BUILTIN_CTLZ] = KW_DEF("clz");
	builtin_list[BUILTIN_CTTZ] = KW_DEF("ctz");
	builtin_list[BUILTIN_EXACT_ADD] = KW_DEF("add");
	builtin_list[BUILTIN_EXACT_DIV] = KW_DEF("div");
	builtin_list[BUILTIN_EXACT_MOD] = KW_DEF("mod");
	builtin_list[BUILTIN_EXACT_MUL] = KW_DEF("mul");
	builtin_list[BUILTIN_EXACT_NEG] = KW_DEF("neg");
	builtin_list[BUILTIN_EXACT_SUB] = KW_DEF("sub");
	builtin_list[BUILTIN_EXP] = KW_DEF("exp");
	builtin_list[BUILTIN_EXP2] = KW_DEF("exp2");
	builtin_list[BUILTIN_EXPECT] = KW_DEF("expect");
	builtin_list[BUILTIN_EXPECT_WITH_PROBABILITY] = KW_DEF("expect_with_probability");
	builtin_list[BUILTIN_FLOOR] = KW_DEF("floor");
	builtin_list[BUILTIN_FMA] = KW_DEF("fma");
	builtin_list[BUILTIN_FMULADD] = KW_DEF("fmuladd");
	builtin_list[BUILTIN_FRAMEADDRESS] = KW_DEF("frameaddress");
	builtin_list[BUILTIN_FSHL] = KW_DEF("fshl");
	builtin_list[BUILTIN_FSHR] = KW_DEF("fshr");
	builtin_list[BUILTIN_GATHER] = KW_DEF("gather");
	builtin_list[BUILTIN_GET_ROUNDING_MODE] = KW_DEF("get_rounding_mode");
	builtin_list[BUILTIN_LOG] = KW_DEF("log");
	builtin_list[BUILTIN_LOG2] = KW_DEF("log2");
	builtin_list[BUILTIN_LOG10] = KW_DEF("log10");
	builtin_list[BUILTIN_MASKED_LOAD] = KW_DEF("masked_load");
	builtin_list[BUILTIN_MASKED_STORE] = KW_DEF("masked_store");
	builtin_list[BUILTIN_MEMCOPY] = KW_DEF("memcpy");
	builtin_list[BUILTIN_MEMCOPY_INLINE] = KW_DEF("memcpy_inline");
	builtin_list[BUILTIN_MEMMOVE] = KW_DEF("memmove");
	builtin_list[BUILTIN_MEMSET] = KW_DEF("memset");
	builtin_list[BUILTIN_MEMSET_INLINE] = KW_DEF("memset_inline");
	builtin_list[BUILTIN_NEARBYINT] = KW_DEF("nearbyint");
	builtin_list[BUILTIN_OVERFLOW_ADD] = KW_DEF("overflow_add");
	builtin_list[BUILTIN_OVERFLOW_SUB] = KW_DEF("overflow_sub");
	builtin_list[BUILTIN_OVERFLOW_MUL] = KW_DEF("overflow_mul");
	builtin_list[BUILTIN_POPCOUNT] = KW_DEF("popcount");
	builtin_list[BUILTIN_POW] = KW_DEF("pow");
	builtin_list[BUILTIN_POW_INT] = KW_DEF("pow_int");
	builtin_list[BUILTIN_PREFETCH] = KW_DEF("prefetch");
	builtin_list[BUILTIN_REDUCE_ADD] = KW_DEF("reduce_add");
	builtin_list[BUILTIN_REDUCE_AND] = KW_DEF("reduce_and");
	builtin_list[BUILTIN_REDUCE_FADD] = KW_DEF("reduce_fadd");
	builtin_list[BUILTIN_REDUCE_FMUL] = KW_DEF("reduce_fmul");
	builtin_list[BUILTIN_REDUCE_MAX] = KW_DEF("reduce_max");
	builtin_list[BUILTIN_REDUCE_MIN] = KW_DEF("reduce_min");
	builtin_list[BUILTIN_REDUCE_MUL] = KW_DEF("reduce_mul");
	builtin_list[BUILTIN_REDUCE_OR] = KW_DEF("reduce_or");
	builtin_list[BUILTIN_REDUCE_XOR] = KW_DEF("reduce_xor");
	builtin_list[BUILTIN_REVERSE] = KW_DEF("reverse");
	builtin_list[BUILTIN_RETURNADDRESS] = KW_DEF("returnaddress");
	builtin_list[BUILTIN_RINT] = KW_DEF("rint");
	builtin_list[BUILTIN_ROUND] = KW_DEF("round");
	builtin_list[BUILTIN_ROUNDEVEN] = KW_DEF("roundeven");
	builtin_list[BUILTIN_SAT_ADD] = KW_DEF("sat_add");
	builtin_list[BUILTIN_SAT_SHL] = KW_DEF("sat_shl");
	builtin_list[BUILTIN_SAT_SUB] = KW_DEF("sat_sub");
	builtin_list[BUILTIN_SCATTER] = KW_DEF("scatter");
	builtin_list[BUILTIN_SELECT] = KW_DEF("select");
	builtin_list[BUILTIN_SET_ROUNDING_MODE] = KW_DEF("set_rounding_mode");
	builtin_list[BUILTIN_SIN] = KW_DEF("sin");
	builtin_list[BUILTIN_SWIZZLE] = KW_DEF("swizzle");
	builtin_list[BUILTIN_SWIZZLE2] = KW_DEF("swizzle2");
	builtin_list[BUILTIN_SQRT] = KW_DEF("sqrt");
	builtin_list[BUILTIN_SYSCALL] = KW_DEF("syscall");
	builtin_list[BUILTIN_SYSCLOCK] = KW_DEF("sysclock");
	builtin_list[BUILTIN_TRAP] = KW_DEF("trap");
	builtin_list[BUILTIN_TRUNC] = KW_DEF("trunc");
	builtin_list[BUILTIN_VECCOMPLT] = KW_DEF("veccomplt");
	builtin_list[BUILTIN_VECCOMPLE] = KW_DEF("veccomple");
	builtin_list[BUILTIN_VECCOMPGT] = KW_DEF("veccompgt");
	builtin_list[BUILTIN_VECCOMPGE] = KW_DEF("veccompge");
	builtin_list[BUILTIN_VECCOMPEQ] = KW_DEF("veccompeq");
	builtin_list[BUILTIN_VECCOMPNE] = KW_DEF("veccompne");
	builtin_list[BUILTIN_UNREACHABLE] = KW_DEF("unreachable");
	builtin_list[BUILTIN_VOLATILE_LOAD] = KW_DEF("volatile_load");
	builtin_list[BUILTIN_VOLATILE_STORE] = KW_DEF("volatile_store");
	builtin_list[BUILTIN_WASM_MEMORY_GROW] = KW_DEF("wasm_memory_grow");
	builtin_list[BUILTIN_WASM_MEMORY_SIZE] = KW_DEF("wasm_memory_size");

	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		assert(builtin_list[i] && "Missing builtin");
	}

	for (unsigned i = 0; i < NUMBER_OF_TYPE_PROPERTIES; i++)
	{
		assert(type_property_list[i] && "Missing type property");
	}

	for (unsigned i = 0; i < NUMBER_OF_BUILTIN_DEFINES; i++)
	{
		assert(builtin_defines[i] && "Missing builtin define");
	}

	type = TOKEN_AT_IDENT;

	kw_at_ensure = KW_DEF("@ensure");
	kw_at_deprecated = KW_DEF("@deprecated");
	kw_at_param = KW_DEF("@param");
	kw_at_pure = KW_DEF("@pure");
	kw_at_require = KW_DEF("@require");
	kw_at_return = KW_DEF("@return");

	attribute_list[ATTRIBUTE_ALIGN] = KW_DEF("@align");
	attribute_list[ATTRIBUTE_BENCHMARK] = KW_DEF("@benchmark");
	attribute_list[ATTRIBUTE_BIGENDIAN] = KW_DEF("@bigendian");
	attribute_list[ATTRIBUTE_BUILTIN] = KW_DEF("@builtin");
	attribute_list[ATTRIBUTE_CALLCONV] = KW_DEF("@callconv");
	attribute_list[ATTRIBUTE_DEFAULT] = KW_DEF("@default");
	attribute_list[ATTRIBUTE_DEPRECATED] = KW_DEF("@deprecated");
	attribute_list[ATTRIBUTE_DYNAMIC] = KW_DEF("@dynamic");
	attribute_list[ATTRIBUTE_EXPORT] = KW_DEF("@export");
	attribute_list[ATTRIBUTE_EXTERN] = KW_DEF("@extern");
	attribute_list[ATTRIBUTE_FINALIZER] = KW_DEF("@finalizer");
	attribute_list[ATTRIBUTE_IF] = KW_DEF("@if");
	attribute_list[ATTRIBUTE_INIT] = KW_DEF("@init");
	attribute_list[ATTRIBUTE_INLINE] = KW_DEF("@inline");
	attribute_list[ATTRIBUTE_LITTLEENDIAN] = KW_DEF("@littleendian");
	attribute_list[ATTRIBUTE_LOCAL] = KW_DEF("@local");
	attribute_list[ATTRIBUTE_MAYDISCARD] = KW_DEF("@maydiscard");
	attribute_list[ATTRIBUTE_NAKED] = KW_DEF("@naked");
	attribute_list[ATTRIBUTE_NODISCARD] = KW_DEF("@nodiscard");
	attribute_list[ATTRIBUTE_NOINIT] = KW_DEF("@noinit");
	attribute_list[ATTRIBUTE_NOINLINE] = KW_DEF("@noinline");
	attribute_list[ATTRIBUTE_NORETURN] = KW_DEF("@noreturn");
	attribute_list[ATTRIBUTE_NOSTRIP] = KW_DEF("@nostrip");
	attribute_list[ATTRIBUTE_OBFUSCATE] = KW_DEF("@obfuscate");
	attribute_list[ATTRIBUTE_OPERATOR] = KW_DEF("@operator");
	attribute_list[ATTRIBUTE_OPTIONAL] = KW_DEF("@optional");
	attribute_list[ATTRIBUTE_OVERLAP] = KW_DEF("@overlap");
	attribute_list[ATTRIBUTE_PACKED] = KW_DEF("@packed");
	attribute_list[ATTRIBUTE_PRIVATE] = KW_DEF("@private");
	attribute_list[ATTRIBUTE_PURE] = kw_at_pure;
	attribute_list[ATTRIBUTE_PUBLIC] = KW_DEF("@public");
	attribute_list[ATTRIBUTE_REFLECT] = KW_DEF("@reflect");
	attribute_list[ATTRIBUTE_SAFEMACRO] = KW_DEF("@safemacro");
	attribute_list[ATTRIBUTE_SECTION] = KW_DEF("@section");
	attribute_list[ATTRIBUTE_TEST] = KW_DEF("@test");
	attribute_list[ATTRIBUTE_UNUSED] = KW_DEF("@unused");
	attribute_list[ATTRIBUTE_USED] = KW_DEF("@used");
	attribute_list[ATTRIBUTE_WASM] = KW_DEF("@wasm");
	attribute_list[ATTRIBUTE_WEAK] = KW_DEF("@weak");
	attribute_list[ATTRIBUTE_WINMAIN] = KW_DEF("@winmain");

	for (unsigned i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
	{
		assert(attribute_list[i] && "Missing attributes");
	}

}


const char *symtab_find(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type)
{
	size_t pos = fnv1hash & symtab.bucket_mask;
	SymtabEntry *bucket = symtab.bucket[pos];
	while (bucket)
	{
		if (bucket->index == fnv1hash && len == bucket->key_len && memcmp(symbol, bucket->symbol, len) == 0)
		{
			*type = bucket->type;
			return bucket->symbol;
		}
		bucket = bucket->next;
	}
	return NULL;
}

const char *symtab_preset(const char *data, TokenType type)
{
	uint32_t len = (uint32_t)strlen(data);
	TokenType result = type;
	const char *res = symtab_add(data, len, fnv1a(data, len), &result);
	assert(result == type);
	return res;
}

const char *symtab_add(const char *data, uint32_t len, uint32_t fnv1hash, TokenType *type)
{
	size_t pos = fnv1hash & symtab.bucket_mask;
	SymtabEntry *first_bucket = symtab.bucket[pos];
	if (!first_bucket)
	{
		SymtabEntry *node = malloc_arena(sizeof(SymtabEntry));
		symtab.bucket[pos] = node;
		node->key_len = len;
		node->next = NULL;
		node->index = fnv1hash;
		node->type = *type;
		return node->symbol = str_copy(data, len);
	}
	SymtabEntry *bucket = first_bucket;
	do
	{
		if (bucket->index == fnv1hash && len == bucket->key_len && memcmp(data, bucket->symbol, len) == 0)
		{
			*type = bucket->type;
			return bucket->symbol;
		}
		bucket = bucket->next;
	} while (bucket);
	SymtabEntry *node = malloc_arena(sizeof(SymtabEntry));
	node->next = first_bucket;
	symtab.bucket[pos] = node;
	node->key_len = len;
	node->index = fnv1hash;
	node->type = *type;
	return node->symbol = str_copy(data, len);
}


void stable_init(STable *table, uint32_t initial_size)
{
	assert(initial_size && "Size must be larger than 0");
	assert (is_power_of_two(initial_size) && "Must be a power of two");

	SEntry *entries = CALLOC(initial_size * sizeof(Entry));
	table->count = 0;
	table->capacity = initial_size;
	table->max_load = initial_size * TABLE_MAX_LOAD;
	table->entries = entries;
}

void stable_clear(STable *table)
{
	memset(table->entries, 0, table->capacity * sizeof(Entry));
	table->count = 0;
}

static inline SEntry *sentry_find(SEntry *entries, uint32_t capacity, const char *key)
{
	uintptr_t hash_key = (uintptr_t)key;
	uint32_t mask = capacity - 1;
	hash_key ^= hash_key >> 16;
	uint32_t index = (uint32_t)hash_key & mask;
	while (1)
	{
		SEntry *entry = &entries[index];
		if (entry->key == key || !entry->key) return entry;
		index = (index + 1) & mask;
	}
}

static inline void stable_resize(STable *table)
{
	ASSERT(table->capacity < MAX_HASH_SIZE, "Table size too large, exceeded max hash size");

	uint32_t new_capacity = table->capacity ? (table->capacity << 2u) : 16u;
	SEntry *new_data = CALLOC(new_capacity * sizeof(SEntry));
	table->count = 0;
	uint32_t len = table->capacity;
	for (uint32_t i = 0; i < len; i++)
	{
		SEntry *entry = &table->entries[i];
		const char *key = entry->key;
		if (!key) continue;
		table->count++;
		SEntry *dest = sentry_find(new_data, new_capacity, key);
		dest->key = key;
		dest->value = entry->value;
	}
	table->entries = new_data;
	table->max_load = new_capacity * TABLE_MAX_LOAD;
	table->capacity = new_capacity;
}

void *stable_set(STable *table, const char *key, void *value)
{
	assert(value && "Cannot insert NULL");
	SEntry *entry = sentry_find(table->entries, table->capacity, key);
	void *old = entry->value;
	if (old == value) return old;
	entry->key = key;
	entry->value = value;
	if (!old)
	{
		table->count++;
		if (table->count >= table->max_load) goto RESIZE;
	}
	return old;
RESIZE:
	stable_resize(table);
	return old;
}


void *stable_get(STable *table, const char *key)
{
	if (!table->entries) return NULL;
	SEntry *entry = sentry_find(table->entries, table->capacity, key);
	return entry->key == NULL ? NULL : entry->value;
}



void htable_init(HTable *table, uint32_t initial_size)
{
	assert(initial_size && "Size must be larger than 0");
	size_t size = next_highest_power_of_2(initial_size);

	size_t mem_size = initial_size * sizeof(HTEntry);
	table->entries = calloc_arena(mem_size);

	// Tap all pages
	char *data = (char *)table->entries;
	for (int i = 0; i < mem_size; i += 4096)
	{
		data[0] = 0;
	}
	table->mask = size - 1;
}

void *htable_set(HTable *table, void *key, void *value)
{
	assert(value && "Cannot insert NULL");
	uint32_t idx = (((uintptr_t)key) ^ ((uintptr_t)key) >> 8) & table->mask;
	HTEntry **entry_ref = &table->entries[idx];
	HTEntry *entry = *entry_ref;
	if (!entry)
	{
		entry = CALLOCS(HTEntry);
		entry->key = key;
		entry->value = value;
		*entry_ref = entry;
		return NULL;
	}
	HTEntry *first_entry = entry;
	do
	{
		if (entry->key == key) return entry->value;
		entry = entry->next;
	} while (entry);

	entry = CALLOCS(HTEntry);
	entry->key = key;
	entry->value = value;
	entry->next = first_entry;
	*entry_ref = entry;
	return NULL;
}


void *htable_get(HTable *table, void *key)
{
	uint32_t idx = (((uintptr_t)key) ^ ((uintptr_t)key) >> 8) & table->mask;
	HTEntry *entry = table->entries[idx];
	if (!entry) return NULL;
	do
	{
		if (entry->key == key) return entry->value;
		entry = entry->next;
	} while (entry);
	return NULL;
}

