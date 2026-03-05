#include "compiler_internal.h"

static inline DeclId *methodentry_find(DeclId *entries, uint32_t capacity, const char *name, Type *type)
{
	uintptr_t hash_key = (uintptr_t)name ^ (uintptr_t)type;
	uint32_t mask = capacity - 1;
	hash_key ^= hash_key >> 16;
	uint32_t index = (uint32_t)hash_key & mask;
	while (1)
	{
		DeclId *entry = &entries[index];
		DeclId decl_id = *entry;
		if (!decl_id) return entry;
		Decl *decl = declptr(*entry);
		if (decl->name == name && typeget(decl->func_decl.type_parent) == type) return entry;
		index = (index + 1) & mask;
	}
}

static inline void methodtable_resize(MethodTable *table)
{
	ASSERT(table->capacity < MAX_HASH_SIZE && "Table size too large, exceeded max hash size");
	uint32_t new_capacity = table->capacity ? (table->capacity << 2u) : 16u;
	DeclId *new_data = CALLOC(new_capacity * sizeof(DeclId));
	table->count = 0;
	uint32_t len = table->capacity;
	for (uint32_t i = 0; i < len; i++)
	{
		DeclId *entry = &table->methods[i];
		DeclId id = *entry;
		if (!id) continue;
		Decl *decl = declptr(id);
		table->count++;
		DeclId *dest = methodentry_find(new_data, new_capacity, decl->name, typeget(decl->func_decl.type_parent));
		*dest = id;
	}
	table->methods = new_data;
	table->max_load = (uint32_t)(new_capacity * TABLE_MAX_LOAD);
	table->capacity = new_capacity;
}

DeclId methodtable_set(MethodTable *table, Decl *decl)
{
	ASSERT(decl && "Cannot insert NULL");
	DeclId *entry = methodentry_find(table->methods, table->capacity, decl->name, typeget(decl->func_decl.type_parent));
	DeclId decl_id = declid(decl);
	DeclId old_id = *entry;
	if (old_id) return old_id;
	*entry = decl_id;
	table->count++;
	if (table->count >= table->max_load) methodtable_resize(table);
	return 0;
}


DeclId methodtable_get(MethodTable *table, Type *type, const char *name)
{
	if (!table->methods) return 0;
	DeclId *entry = methodentry_find(table->methods, table->capacity, name, type);
	return *entry;
}

void methodtable_init(MethodTable *table, uint32_t initial_size)
{
	ASSERT(initial_size && "Size must be larger than 0");
	assert (is_power_of_two(initial_size) && "Must be a power of two");

	DeclId *entries = CALLOC(initial_size * sizeof(DeclId));
	table->count = 0;
	table->capacity = initial_size;
	table->max_load = (uint32_t)(initial_size * TABLE_MAX_LOAD);
	table->methods = entries;
}
