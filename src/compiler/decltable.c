#include "compiler_internal.h"

static inline DeclId *declentry_find(DeclId *entries, uint32_t capacity, const char *name)
{
	uintptr_t hash_key = (uintptr_t)name;
	uint32_t mask = capacity - 1;
	hash_key ^= hash_key >> 16;
	uint32_t index = (uint32_t)hash_key & mask;
	while (1)
	{
		DeclId *entry = &entries[index];
		DeclId decl_id = *entry;
		if (!decl_id) return entry;
		Decl *decl = declptr(*entry);
		if (decl->name == name) return entry;
		index = (index + 1) & mask;
	}
}

static inline void decltable_resize(DeclTable *table)
{
	ASSERT(table->capacity < MAX_HASH_SIZE, "Table size too large, exceeded max hash size");
	uint32_t new_capacity = table->capacity ? (table->capacity << 2u) : 16u;
	DeclId *new_data = CALLOC(new_capacity * sizeof(DeclId));
	table->count = 0;
	uint32_t len = table->capacity;
	for (uint32_t i = 0; i < len; i++)
	{
		DeclId *entry = &table->entries[i];
		DeclId id = *entry;
		if (!id) continue;
		Decl *decl = declptr(id);
		table->count++;
		DeclId *dest = declentry_find(new_data, new_capacity, decl->name);
		*dest = id;
	}
	table->entries = new_data;
	table->max_load = (uint32_t)(new_capacity * TABLE_MAX_LOAD);
	table->capacity = new_capacity;
}

void decltable_set(DeclTable *table, Decl *decl)
{
	ASSERT0(decl && "Cannot insert NULL");
	DeclId *entry = declentry_find(table->entries, table->capacity, decl->name);
	DeclId decl_id = declid(decl);
	DeclId old_id = *entry;
	ASSERT0(old_id != decl_id);
	// Simple case, a new decl
	if (!old_id)
	{
		DEBUG_LOG("Adding unique %s", decl->name);
		*entry = decl_id;
		table->count++;
		if (table->count >= table->max_load) decltable_resize(table);
		return;
	}
	Decl *old = declptr(old_id);
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
	*entry = declid(list);
}


DeclId decltable_get(DeclTable *table, const char *name)
{
	if (!table->entries) return 0;
	DeclId *entry = declentry_find(table->entries, table->capacity, name);
	return *entry;
}

void decltable_init(DeclTable *table, uint32_t initial_size)
{
	ASSERT0(initial_size && "Size must be larger than 0");
	assert (is_power_of_two(initial_size) && "Must be a power of two");

	DeclId *entries = CALLOC(initial_size * sizeof(DeclId));
	table->count = 0;
	table->capacity = initial_size;
	table->max_load = (uint32_t)(initial_size * TABLE_MAX_LOAD);
	table->entries = entries;
}