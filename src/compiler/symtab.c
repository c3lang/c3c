// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
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

const char *kw_std__core;
const char *kw_std__core__types;
const char *kw_typekind;

const char *kw_in;
const char *kw_out;
const char *kw_inout;
const char *kw_align;
const char *kw_deprecated;
const char *kw_distinct;
const char *kw_elements;
const char *kw_return;
const char *kw_type;
const char *kw_inf;
const char *kw_inline;
const char *kw_kind;
const char *kw_elementat;
const char *kw_elementref;
const char *kw_elementset;
const char *kw_len;
const char *kw_main;
const char *kw_max;
const char *kw_min;
const char *kw_nan;
const char *kw_noinline;
const char *kw_ordinal;
const char *kw_ptr;
const char *kw_pure;
const char *kw_std;
const char *kw_sizeof;
const char *kw_values;
const char *kw_FILE;
const char *kw_FUNC;
const char *kw_LINE;
const char *kw_LINEREAL;
const char *kw_incr;
const char *kw_check_assign;
const char *kw_argc;
const char *kw_argv;
const char *kw_mainstub;;
const char *kw_at_ensure;
const char *kw_at_require;
const char *kw_at_pure;
const char *kw_at_optreturn;
const char *kw_at_param;
const char *kw_at_return;
const char *kw_at_checked;

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
	for (int i = 0; i < TOKEN_LAST; i++)
	{
		const char* name = token_type_to_string((TokenType)i);
		// Skip non-keywords
		if (!char_is_lower(name[0]))
		{
			if ((name[0] != '@' && name[0] != '$') || !char_is_lower(name[1])) continue;
		}
		uint32_t len = (uint32_t)strlen(name);
		TokenType type = (TokenType)i;
		const char* interned = symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type);
		if (type == TOKEN_RETURN) kw_return = interned;
		assert(type == (TokenType)i);
		assert(symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type) == interned);
	}

	// Init some constant idents
#define KW_DEF(x) symtab_add(x, sizeof(x) - 1, fnv1a(x, sizeof(x) - 1), &type)
	TokenType type = TOKEN_CONST_IDENT;
	kw_LINE = KW_DEF("LINE");
	kw_LINEREAL = KW_DEF("LINEREAL");
	kw_FILE = KW_DEF("FILE");
	kw_FUNC = KW_DEF("FUNC");

	type = TOKEN_TYPE_IDENT;
	kw_typekind = KW_DEF("TypeKind");

	type = TOKEN_IDENT;
	kw_std__core = KW_DEF("std::core");
	kw_std__core__types = KW_DEF("std::core::types");
	kw_sizeof = KW_DEF("sizeof");
	kw_in = KW_DEF("in");
	kw_out = KW_DEF("out");
	kw_inout = KW_DEF("inout");
	kw_align = KW_DEF("align");
	kw_deprecated = KW_DEF("deprecated");
	kw_distinct = KW_DEF("distinct");
	kw_elements = KW_DEF("elements");
	kw_type = KW_DEF("type");
	kw_inf = KW_DEF("inf");
	kw_inline = KW_DEF("inline");
	kw_kind = KW_DEF("kind");
	kw_elementat = KW_DEF("elementat");
	kw_elementref = KW_DEF("elementref");
	kw_elementset = KW_DEF("elementset");
	kw_len = KW_DEF("len");
	kw_main = KW_DEF("main");
	kw_max = KW_DEF("max");
	kw_min = KW_DEF("min");
	kw_nan = KW_DEF("nan");
	kw_noinline = KW_DEF("noinline");
	kw_ordinal = KW_DEF("ordinal");
	kw_ptr = KW_DEF("ptr");
	kw_pure = KW_DEF("pure");
	kw_std = KW_DEF("std");
	kw_values = KW_DEF("values");
	kw_incr = KW_DEF("incr");
	kw_check_assign = KW_DEF("check_assign");

	kw_argc = KW_DEF("_$argc");
	kw_argv = KW_DEF("_$argv");
	kw_mainstub = KW_DEF("_$mainstub");

	builtin_list[BUILTIN_TRAP] = KW_DEF("trap");
	builtin_list[BUILTIN_UNREACHABLE] = KW_DEF("unreachable");
	builtin_list[BUILTIN_STACKTRACE] = KW_DEF("stacktrace");
	builtin_list[BUILTIN_CEIL] = KW_DEF("ceil");
	builtin_list[BUILTIN_TRUNC] = KW_DEF("trunc");
	builtin_list[BUILTIN_SIN] = KW_DEF("sin");
	builtin_list[BUILTIN_COS] = KW_DEF("cos");
	builtin_list[BUILTIN_LOG] = KW_DEF("log");
	builtin_list[BUILTIN_LOG2] = KW_DEF("log2");
	builtin_list[BUILTIN_LOG10] = KW_DEF("log10");
	builtin_list[BUILTIN_SQRT] = KW_DEF("sqrt");
	builtin_list[BUILTIN_POW] = KW_DEF("pow");
	builtin_list[BUILTIN_EXP] = KW_DEF("exp");
	builtin_list[BUILTIN_MAX] = KW_DEF("max");
	builtin_list[BUILTIN_MIN] = KW_DEF("min");
	builtin_list[BUILTIN_FMA] = KW_DEF("fma");
	builtin_list[BUILTIN_FABS] = KW_DEF("fabs");
	builtin_list[BUILTIN_VOLATILE_STORE] = KW_DEF("volatile_store");
	builtin_list[BUILTIN_VOLATILE_LOAD] = KW_DEF("volatile_load");
	builtin_list[BUILTIN_MEMCOPY] = KW_DEF("memcpy");
	builtin_list[BUILTIN_MEMSET] = KW_DEF("memset");

	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		assert(builtin_list[i] && "Missing builtin");
	}

	type = TOKEN_AT_IDENT;
	kw_at_param = KW_DEF("@param");
	kw_at_ensure = KW_DEF("@ensure");
	kw_at_optreturn = KW_DEF("@optreturn");
	kw_at_pure = KW_DEF("@pure");
	kw_at_require = KW_DEF("@require");
	kw_at_checked = KW_DEF("@checked");
	kw_at_return = KW_DEF("@return");
	attribute_list[ATTRIBUTE_INLINE] = KW_DEF("@inline");
	attribute_list[ATTRIBUTE_NOINLINE] = KW_DEF("@noinline");
	attribute_list[ATTRIBUTE_BIGENDIAN] = KW_DEF("@bigendian");
	attribute_list[ATTRIBUTE_LITTLEENDIAN] = KW_DEF("@littleendian");
	attribute_list[ATTRIBUTE_NORETURN] = KW_DEF("@noreturn");
	attribute_list[ATTRIBUTE_SECTION] = KW_DEF("@section");
	attribute_list[ATTRIBUTE_EXTNAME] = KW_DEF("@extname");
	attribute_list[ATTRIBUTE_WEAK] = KW_DEF("@weak");
	attribute_list[ATTRIBUTE_ALIGN] = KW_DEF("@align");
	attribute_list[ATTRIBUTE_PACKED] = KW_DEF("@packed");
	attribute_list[ATTRIBUTE_UNUSED] = KW_DEF("@unused");
	attribute_list[ATTRIBUTE_USED] = KW_DEF("@used");
	attribute_list[ATTRIBUTE_NAKED] = KW_DEF("@naked");
	attribute_list[ATTRIBUTE_CDECL] = KW_DEF("@cdecl");
	attribute_list[ATTRIBUTE_STDCALL] = KW_DEF("@stdcall");
	attribute_list[ATTRIBUTE_VECCALL] = KW_DEF("@veccall");
	attribute_list[ATTRIBUTE_REGCALL] = KW_DEF("@regcall");
	attribute_list[ATTRIBUTE_FASTCALL] = KW_DEF("@fastcall");
	attribute_list[ATTRIBUTE_OVERLAP] = KW_DEF("@overlap");
	attribute_list[ATTRIBUTE_OPERATOR] = KW_DEF("@operator");
	attribute_list[ATTRIBUTE_PURE] = kw_at_pure;
	attribute_list[ATTRIBUTE_REFLECT] = KW_DEF("@reflect");
	attribute_list[ATTRIBUTE_AUTOIMPORT] = KW_DEF("@autoimport");

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

void *htable_set(HTable *table, const char *key, void *value)
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


void *htable_get(HTable *table, const char *key)
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

