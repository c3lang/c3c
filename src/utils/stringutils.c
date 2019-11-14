// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

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
	return buffer;
}