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
	if (len < 1) return "";
	va_start(list, var);
	char *buffer = malloc_arena((uint32_t)len + 1);
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

void slicetrim(StringSlice *slice)
{
	size_t i;
	for (i = 0; i < slice->len; i++)
	{
		if (slice->ptr[i] != ' ') break;
	}
	slice->ptr += i;
	slice->len -= i;
	for (i = slice->len; i > 0; i--)
	{
		if (slice->ptr[i - 1] != ' ') break;
	}
	slice->len = i;
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
	unsigned a_len = (unsigned)strlen(a);
	unsigned b_len = (unsigned)strlen(b);
	char *buffer = malloc_arena(a_len + b_len + 1);
	memcpy(buffer, a, a_len);
	memcpy(buffer + a_len, b, b_len);
	buffer[a_len + b_len] = '\0';
	return buffer;
}

#if PLATFORM_WINDOWS

int asprintf(char **strp, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	int res = vasprintf(strp, fmt, args);
	va_end(args);
	return res;
}

int vasnprintf(char **strp, const char *fmt, va_list args)
{
	va_list args_copy;
	va_copy(args_copy, args);
	int res = vsprintf(NULL, fmt, args);
	if (res < 0) goto END;
	char *buf = ccalloc(res + 1, 1);
	if (NULL == buf) goto END;
	sprintf(buf, fmt, args_copy);  // this can't fail, right?
	*strp = buf;
	END:
	va_end(args_copy);
	return res;
}

int vasprintf(char** ret, const char* fmt, va_list args) {
	int length = _vsnprintf(NULL, 0, fmt, args);
	if (length < 0) { // check if _vsnprintf failed
		return -1;
	}
	*ret = cmalloc(length + 1);
	if (!*ret) { // check if malloc failed
		return -1;
	}
	// Write String 
	_vsnprintf(*ret, length + 1, fmt, args);
	(*ret)[length] = '\0'; // make sure there is a null terminator
	return length;
}

char *strndup(const char *s, size_t n)
{
	n = strnlen(s, n);
	char *t = ccalloc(n + 1, 1);
	if (NULL != t)
	{
		memcpy(t, s, n);
		t[n] = '\0';
	}
	return t;
}

#endif
