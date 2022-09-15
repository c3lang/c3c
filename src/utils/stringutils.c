// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include <stdarg.h>
#include <stddef.h>
#include <assert.h>
#include "lib.h"
#include "stdio.h"

struct ScratchBuf scratch_buffer;

int str_findlist(const char *value, unsigned count, const char** elements)
{
	for (unsigned i = 0; i < count; i++)
	{
		if (strcmp(value, elements[i]) == 0) return (int)i;
	}
	return -1;
}

bool str_has_no_uppercase(const char *string)
{
	char c;
	while ((c = *(string++)) != '\0')
	{
		if (char_is_upper(c)) return false;
	}
	return true;
}

bool str_is_valid_lowercase_name(const char *string)
{
	char c;
	// Must start with a lower case
	if (!char_is_lower(string[0])) return false;
	int length = 0;
	while ((c = *(string++)) != '\0')
	{
		if (!char_is_lower_alphanum_(c)) return false;
		if (++length > 127) return false;
	}
	return true;
}

void str_ellide_in_place(char *string, size_t max_size_shown)
{
	size_t len = strlen(string);
	if (max_size_shown > len) return;
	for (int i = 0; i < 3; i++)
	{
		string[max_size_shown - i] = '.';
	}
	string[max_size_shown + 1] = 0;
}

char *str_vprintf(const char *var, va_list list)
{
	va_list copy;
	va_copy(copy, list);
	int len = vsnprintf(NULL, 0, var, copy);
	va_end(copy);
	if (len < 1)
	{
		return "";
	}
	char *buffer = malloc_string((uint32_t)len + 1);
	int new_len = vsnprintf(buffer, len + 1, var, list);
	assert(len == new_len);
	(void)new_len;
	return buffer;
}

char *str_printf(const char *var, ...)
{
	va_list list;
	va_start(list, var);
	char *res = str_vprintf(var, list);
	va_end(list);
	return res;
}

const char *str_remove_suffix(const char *name, const char *suffix)
{
	size_t name_len = strlen(name);
	size_t suffix_len = strlen(suffix);
	if (name_len <= suffix_len) return NULL;
	if (memcmp(name + name_len - suffix_len, suffix, suffix_len) != 0) return NULL;
	size_t result_len = name_len - suffix_len;
	char *name_copy = malloc_string(result_len + 1);
	memcpy(name_copy, name, result_len);
	name_copy[result_len] = 0;
	return name_copy;
}

bool str_has_suffix(const char *name, const char *suffix)
{
	size_t name_len = strlen(name);
	size_t suffix_len = strlen(suffix);
	if (name_len <= suffix_len) return false;
	return memcmp(name + name_len - suffix_len, suffix, suffix_len) == 0;
}


StringSlice slice_next_token(StringSlice *slice, char separator)
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

void slice_trim(StringSlice *slice)
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

char *str_trim(char *str)
{
	str_trim_end(str);
	return (char *)str_trim_start(str);
}

const char *str_trim_start(const char *str)
{
	while (str[0] != 0)
	{
		switch (str[0])
		{
			case ' ':
			case '\t':
			case '\n':
			case '\r':
				str++;
				continue;
			default:
				break;
		}
		break;
	}
	return str;
}

void str_trim_end(char *str)
{
	size_t len = strlen(str);
	char *end = str + len - 1;
	while (len > 0)
	{
		switch (*end)
		{
			case ' ':
			case '\t':
			case '\n':
			case '\r':
				len--;
				end--;
				continue;
			default:
				end[1] = 0;
				return;
		}
	}
}
char *str_cat(const char *a, const char *b)
{
	unsigned a_len = (unsigned)strlen(a);
	unsigned b_len = (unsigned)strlen(b);
	char *buffer = malloc_string(a_len + b_len + 1);
	memcpy(buffer, a, a_len);
	memcpy(buffer + a_len, b, b_len);
	buffer[a_len + b_len] = '\0';
	return buffer;
}

char *str_copy(const char *start, size_t str_len)
{
	char *dst = calloc_string(str_len + 1);
	memcpy(dst, start, str_len);
	// No need to set the end
	return dst;
}

void scratch_buffer_clear(void)
{
	scratch_buffer.len = 0;
}

void scratch_buffer_append_len(const char *string, size_t len)
{
	if (len + scratch_buffer.len > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	memcpy(scratch_buffer.str + scratch_buffer.len, string, len);
	scratch_buffer.len += (uint32_t)len;
}

void scratch_buffer_append(const char *string)
{
	scratch_buffer_append_len(string, strlen(string));
}

void scratch_buffer_append_signed_int(int64_t i)
{
	uint32_t len_needed = (uint32_t)sprintf(&scratch_buffer.str[scratch_buffer.len], "%lld", (long long)i);
	if (scratch_buffer.len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	scratch_buffer.len += len_needed;
}

void scratch_buffer_append_double(double d)
{
	uint32_t len_needed = (uint32_t)sprintf(&scratch_buffer.str[scratch_buffer.len], "%f", d);
	if (scratch_buffer.len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	scratch_buffer.len += len_needed;

	//removing unused zeroes and dot
	size_t i = scratch_buffer.len;
	while (scratch_buffer.len > 0)
	{
		if (scratch_buffer.str[scratch_buffer.len - 1] != '0' && scratch_buffer.str[scratch_buffer.len - 1] != '.')
		{
			return;
		}
		scratch_buffer.len--;
	}
}

void scratch_buffer_append_unsigned_int(uint64_t i)
{
	uint32_t len_needed = (uint32_t)sprintf(&scratch_buffer.str[scratch_buffer.len], "%llu", (unsigned long long)i);
	if (scratch_buffer.len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	scratch_buffer.len += len_needed;
}

void scratch_buffer_printf(const char *format, ...)
{
	va_list args;
	va_start(args, format);
	uint32_t len_needed = (uint32_t)vsprintf(&scratch_buffer.str[scratch_buffer.len], format, args);
	if (scratch_buffer.len + len_needed > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}
	va_end(args);
	scratch_buffer.len += len_needed;
}

void scratch_buffer_append_char(char c)
{
	if (scratch_buffer.len + 1 > MAX_STRING_BUFFER - 1)
	{
		error_exit("Scratch buffer size (%d chars) exceeded", MAX_STRING_BUFFER - 1);
	}

	scratch_buffer.str[scratch_buffer.len++] = c;
}

char *scratch_buffer_to_string(void)
{
	scratch_buffer.str[scratch_buffer.len] = '\0';
	return scratch_buffer.str;
}

char *scratch_buffer_copy(void)
{
	return str_copy(scratch_buffer.str, scratch_buffer.len);
}


