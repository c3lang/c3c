// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <stdarg.h>
#include <stddef.h>
#include <assert.h>
#include "lib.h"
#include "stdio.h"

char *strformat(const char *var, ...)
{
	va_list list;
	va_start(list, var);
	int len = vsnprintf(NULL, 0, var, list);
	va_end(list);
	if (len == 0) return "";
	va_start(list, var);
	char *buffer = malloc_arena(len + 1);
	int new_len = vsnprintf(buffer, len + 1, var, list);
	va_end(list);
	assert(len == new_len);
	(void)new_len;
	return buffer;
}

StringSlice strnexttok(StringSlice *slice, char separator)
{
	for (size_t i = 0; i < slice->len; i++)
	{
		if (slice->ptr[i] == separator)
		{
			StringSlice result = { slice->ptr, i };
			slice->ptr = slice->ptr + i + 1;
			slice->len = slice->len - i - 1;
			return result;
		}
	}
	StringSlice result = *slice;
	slice->ptr = slice->ptr + slice->len;
	slice->len = 0;
	return result;
}

char *strcopy(const char *start, size_t len)
{
	char *buffer = malloc_arena(len + 1);
	memcpy(buffer, start, len);
	buffer[len] = '\0';
	return buffer;
}

char *strcat_arena(const char *a, const char *b)
{
	unsigned a_len = strlen(a);
	unsigned b_len = strlen(b);
	char *buffer = malloc_arena(a_len + b_len + 1);
	memcpy(buffer, a, a_len);
	memcpy(buffer + a_len, b, b_len);
	buffer[a_len + b_len] = '\0';
	return buffer;
}