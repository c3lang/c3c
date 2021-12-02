// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"

#define TABLE_MAX_LOAD 0.75
#define MAX_HASH_SIZE (1024 * 1024)


typedef struct _SymEntry
{
	const char *value;
	TokenType type;
	uint32_t key_len;
	uint32_t hash;
} SymEntry;

typedef struct _SymTab
{
	uint32_t count;
	uint32_t capacity;
	SymEntry *entries;
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

const char *kw_align;
const char *kw_deprecated;
const char *kw_distinct;
const char *kw_ensure;
const char *kw_elements;
const char *kw_errors;
const char *kw_inf;
const char *kw_inline;
const char *kw_iterator;
const char *kw_operator_element_at;
const char *kw_operator_element_at_ref;
const char *kw_operator_len;
const char *kw_len;
const char *kw_main;
const char *kw_max;
const char *kw_min;
const char *kw_next;
const char *kw_nan;
const char *kw_ordinal;
const char *kw_param;
const char *kw_pure;
const char *kw_reqparse;
const char *kw_require;
const char *kw_std;
const char *kw___ceil;
const char *kw___round;
const char *kw___sqrt;
const char *kw___trunc;
const char *kw_FILE;
const char *kw_FUNC;
const char *kw_LINE;
const char *kw_LINEREAL;
const char *kw_default_iterator;
const char *kw_incr;
const char *kw_check_assign;
const char *kw_builtin_ceil;
const char *kw_builtin_trunc;
const char *kw_builtin_sqrt;
const char *kw_builtin_cos;
const char *kw_builtin_sin;
const char *kw_builtin_log;
const char *kw_builtin_log2;
const char *kw_builtin_log10;
const char *kw_builtin_max;
const char *kw_builtin_min;
const char *kw_builtin_pow;
const char *kw_builtin_exp;
const char *kw_builtin_fabs;
const char *kw_builtin_fma;
const char *kw_builtin_cmpxchg;

void symtab_init(uint32_t capacity)
{
	assert (is_power_of_two(capacity) && "Must be a power of two");
	size_t size = capacity *sizeof(SymEntry);
	symtab.entries = MALLOC(size);
	memset(symtab.entries, 0, size);
	symtab.count = 0;
	symtab.capacity = capacity;

	// Add keywords.
	for (int i = 0; i < TOKEN_LAST; i++)
	{
		const char* name = token_type_to_string((TokenType)i);
		// Skip non-keywords
		if (!is_lower(name[0]))
		{
			if ((name[0] != '@' && name[0] != '$') || !is_lower(name[1])) continue;
		}
		uint32_t len = (uint32_t)strlen(name);
		TokenType type = (TokenType)i;
		const char* interned = symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type);
		assert(type == (TokenType)i);
		assert(symtab_add(name, (uint32_t)strlen(name), fnv1a(name, len), &type) == interned);
	}

	// Init some constant idents
	TokenType type = TOKEN_IDENT;
#define KW_DEF(x) symtab_add(x, sizeof(x) - 1, fnv1a(x, sizeof(x) - 1), &type)
	kw_align = KW_DEF("align");
	kw_deprecated = KW_DEF("deprecated");
	kw_distinct = KW_DEF("distinct");
	kw_elements = KW_DEF("elements");
	kw_ensure = KW_DEF("ensure");
	kw_errors = KW_DEF("errors");
	kw_inf = KW_DEF("inf");
	kw_inline = KW_DEF("inline");
	kw_iterator = KW_DEF("iterator");
	kw_operator_element_at = KW_DEF("operator_element_at");
	kw_operator_element_at_ref = KW_DEF("operator_element_at_ref");
	kw_operator_len = KW_DEF("operator_len");
	kw_len = KW_DEF("len");
	kw_main = KW_DEF("main");
	kw_max = KW_DEF("max");
	kw_min = KW_DEF("min");
	kw_nan = KW_DEF("nan");
	kw_next = KW_DEF("next");
	kw_ordinal = KW_DEF("ordinal");
	kw_param = KW_DEF("param");
	kw_pure = KW_DEF("pure");
	kw_require = KW_DEF("require");
	kw_std = KW_DEF("std");
	kw___ceil = KW_DEF("__ceil");
	kw___round = KW_DEF("__round");
	kw___sqrt = KW_DEF("__sqrt");
	kw___trunc = KW_DEF("__trunc");
	kw_LINE = KW_DEF("LINE");
	kw_LINEREAL = KW_DEF("LINEREAL");
	kw_FILE = KW_DEF("FILE");
	kw_FUNC = KW_DEF("FUNC");
	kw_incr = KW_DEF("incr");
	kw_default_iterator = KW_DEF("default_iterator");
	kw_check_assign = KW_DEF("check_assign");

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

	for (unsigned i = 0; i < NUMBER_OF_BUILTINS; i++)
	{
		assert(builtin_list[i] && "Missing builtin");
	}

	attribute_list[ATTRIBUTE_INLINE] = kw_inline;
	attribute_list[ATTRIBUTE_NOINLINE] = KW_DEF("noinline");
	attribute_list[ATTRIBUTE_OPAQUE] = KW_DEF("opaque");
	attribute_list[ATTRIBUTE_BIGENDIAN] = KW_DEF("bigendian");
	attribute_list[ATTRIBUTE_LITTLEENDIAN] = KW_DEF("littleendian");
	attribute_list[ATTRIBUTE_NORETURN] = KW_DEF("noreturn");
	attribute_list[ATTRIBUTE_SECTION] = KW_DEF("section");
	attribute_list[ATTRIBUTE_EXTNAME] = KW_DEF("extname");
	attribute_list[ATTRIBUTE_WEAK] = KW_DEF("weak");
	attribute_list[ATTRIBUTE_ALIGN] = kw_align;
	attribute_list[ATTRIBUTE_PACKED] = KW_DEF("packed");
	attribute_list[ATTRIBUTE_UNUSED] = KW_DEF("unused");
	attribute_list[ATTRIBUTE_USED] = KW_DEF("used");
	attribute_list[ATTRIBUTE_NAKED] = KW_DEF("naked");
	attribute_list[ATTRIBUTE_CDECL] = KW_DEF("cdecl");
	attribute_list[ATTRIBUTE_STDCALL] = KW_DEF("stdcall");
	attribute_list[ATTRIBUTE_VECCALL] = KW_DEF("veccall");
	attribute_list[ATTRIBUTE_REGCALL] = KW_DEF("regcall");
	attribute_list[ATTRIBUTE_FASTCALL] = KW_DEF("fastcall");
	attribute_list[ATTRIBUTE_OVERLAP] = KW_DEF("overlap");

	for (unsigned i = 0; i < NUMBER_OF_ATTRIBUTES; i++)
	{
		assert(attribute_list[i] && "Missing attributes");
	}

}

static inline SymEntry *entry_find(const char *key, uint32_t key_len, uint32_t hash)
{
	uint32_t index = hash & (symtab.capacity - 1);
	while (1)
	{
		SymEntry *entry = &symtab.entries[index];
		if (entry->key_len == key_len && (entry->value == key || memcmp(key, entry->value, key_len) == 0)) return entry;
		if (entry->value == NULL)
		{
			return entry;
		}
		index = (index + 1) % (symtab.capacity - 1);
	}
}

const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type)
{
	if (symtab.count + 1 > symtab.capacity * TABLE_MAX_LOAD)
	{
		FATAL_ERROR("Symtab exceeded capacity, please increase --symtab.");
	}
	SymEntry *entry = entry_find(symbol, len, fnv1hash);
	if (entry->value)
	{
		*type = entry->type;
		return entry->value;
	}

	char *copy = MALLOC(len + 1);
	memcpy(copy, symbol, len);
	copy[len] = '\0';
	entry->value = copy;
	entry->key_len = len;
	entry->hash = fnv1hash;
	entry->type = *type;
	symtab.count++;
	return entry->value;
}

const char *symtab_find(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type)
{
	SymEntry *entry = entry_find(symbol, len, fnv1hash);
	if (!entry->value) return NULL;
	*type = entry->type;
	return entry->value;
}

void stable_init(STable *table, uint32_t initial_size)
{
	assert(initial_size && "Size must be larger than 0");
	assert (is_power_of_two(initial_size) && "Must be a power of two");

	SEntry *entries = MALLOC(initial_size * sizeof(Entry));
	for (uint32_t i = 0; i < initial_size; i++)
	{
		entries[i].key = NULL;
		entries[i].value = NULL;
	}
	table->count = 0;
	table->capacity = initial_size;
	table->entries = entries;
}

void stable_clear(STable *table)
{
	memset(table->entries, 0, table->capacity * sizeof(Entry));
	table->count = 0;
}

#define TOMBSTONE ((void *)0x01)
static SEntry *sentry_find(SEntry *entries, uint32_t capacity, const char *key)
{
	uint32_t index = (uint32_t)((((uintptr_t)key) >> 2u) & (capacity - 1));
	SEntry *tombstone = NULL;
	while (1)
	{
		SEntry *entry = &entries[index];
		if (entry->key == key) return entry;
		if (entry->key == NULL)
		{
			if (entry->value != TOMBSTONE)
			{
				return tombstone ? tombstone : entry;
			}
			else
			{
				if (!tombstone) tombstone = entry;
			}
		}
		index = (index + 1) & (capacity - 1);
	}
}


void *stable_set(STable *table, const char *key, void *value)
{
	assert(value && "Cannot insert NULL");
	if (table->count + 1 > table->capacity * TABLE_MAX_LOAD)
	{
		ASSERT(table->capacity < MAX_HASH_SIZE, "Table size too large, exceeded %d", MAX_HASH_SIZE);

		uint32_t new_capacity = table->capacity ? (table->capacity << 1u) : 16u;
		SEntry *new_data = MALLOC(new_capacity * sizeof(SEntry));
		for (uint32_t i = 0; i < new_capacity; i++)
		{
			new_data[i].key = NULL;
			new_data[i].value = NULL;
		}
		table->count = 0;
		for (uint32_t i = 0; i < table->capacity; i++)
		{
			SEntry *entry = &table->entries[i];
			if (!entry->key) continue;
			table->count++;
			SEntry *dest = sentry_find(new_data, new_capacity, entry->key);
			*dest = *entry;
		}
		table->entries = new_data;
		table->capacity = new_capacity;
	}

	SEntry *entry = sentry_find(table->entries, table->capacity, key);
	void *old = entry->value && entry->value != TOMBSTONE ? entry->value : NULL;
	entry->key = key;
	entry->value = value;
	if (!old) table->count++;
	return old;
}


void *stable_get(STable *table, const char *key)
{
	if (!table->entries) return NULL;
	SEntry *entry = sentry_find(table->entries, table->capacity, key);
	return entry->key == NULL ? NULL : entry->value;
}

void *stable_delete(STable *table, const char *key)
{
	if (!table->count) return NULL;
	SEntry *entry = sentry_find(table->entries, table->capacity, key);
	if (!entry->key) return NULL;
	void *value = entry->value;
	entry->key = NULL;
	entry->value = TOMBSTONE;
	return value;
}
