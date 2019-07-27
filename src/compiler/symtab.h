#pragma once

// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include <stdint.h>
#include "tokens.h"

void symtab_init(uint32_t max_size);
const char *symtab_add(const char *symbol, uint32_t len, uint32_t fnv1hash, TokenType *type);

typedef struct _VoidEntry
{
	const char *key;
	void *value;
} SEntry;

typedef struct _STable
{
	uint32_t count;
	uint32_t capacity;
	SEntry *entries;
} STable;

void stable_init(STable *table, uint32_t initial_size);
void *stable_set(STable *table, const char *key, void *value);
void *stable_get(STable *table, const char *key);
void *stable_delete(STable *table, const char *key);
void stable_clear(STable *table);
