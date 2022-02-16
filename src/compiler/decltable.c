#include "compiler_internal.h"

static inline Decl **declentry_find(Decl **entries, uint32_t capacity, const char *name)
{
	uintptr_t hash_key = (uintptr_t)name;
	uint32_t mask = capacity - 1;
	hash_key ^= hash_key >> 16;
	uint32_t index = (uint32_t)hash_key & mask;
	while (1)
	{
		Decl **entry = &entries[index];
		Decl *decl = *entry;
		if (!decl || decl->name == name) return entry;
		index = (index + 1) & mask;
	}
}

static inline void decltable_resize(DeclTable *table)
{
	ASSERT(table->capacity < MAX_HASH_SIZE, "Table size too large, exceeded max hash size");
	uint32_t new_capacity = table->capacity ? (table->capacity << 2u) : 16u;
	Decl **new_data = CALLOC(new_capacity * sizeof(Decl*));
	table->count = 0;
	uint32_t len = table->capacity;
	for (uint32_t i = 0; i < len; i++)
	{
		Decl **entry = &table->entries[i];
		Decl *decl = *entry;
		if (!decl) continue;
		table->count++;
		Decl **dest = declentry_find(new_data, new_capacity, decl->name);
		*dest = decl;
	}
	table->entries = new_data;
	table->max_load = new_capacity * TABLE_MAX_LOAD;
	table->capacity = new_capacity;
}

void decltable_set(DeclTable *table, Decl *decl)
{
	assert(decl && "Cannot insert NULL");
	Decl **entry = declentry_find(table->entries, table->capacity, decl->name);
	Decl *old = *entry;
	assert(old != decl);
	// Simple case, a new decl
	if (!old)
	{
		DEBUG_LOG("Adding unique %s", decl->name);
		*entry = decl;
		table->count++;
		if (table->count >= table->max_load) decltable_resize(table);
		return;
	}
	if (old->decl_kind == DECL_DECLARRAY)
	{
		DEBUG_LOG("Adding %s to %s", decl->name, old->name);
		vec_add(old->decl_list, decl);
		return;
	}
	Decl *list = decl_calloc();
	list->decl_kind = DECL_DECLARRAY;
	list->name = decl->name;
	vec_add(list->decl_list, old);
	vec_add(list->decl_list, decl);
	DEBUG_LOG("Adding list of %s and %s", decl->name, old->name);
	*entry = list;
}


Decl *decltable_get(DeclTable *table, const char *name)
{
	if (!table->entries) return NULL;
	Decl **entry = declentry_find(table->entries, table->capacity, name);
	return *entry;
}

void decltable_init(DeclTable *table, uint32_t initial_size)
{
	assert(initial_size && "Size must be larger than 0");
	assert (is_power_of_two(initial_size) && "Must be a power of two");

	Decl **entries = CALLOC(initial_size * sizeof(Decl*));
	table->count = 0;
	table->capacity = initial_size;
	table->max_load = initial_size * TABLE_MAX_LOAD;
	table->entries = entries;
}