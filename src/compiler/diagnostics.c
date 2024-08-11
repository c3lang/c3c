// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <math.h>




#define LINES_SHOWN 4
#define MAX_WIDTH 120

static void print_error_type_at(SourceSpan location, const char *message, PrintType print_type)
{
	if (!location.a)
	{
		eprintf("Unlocalized error: %s.\n", message);
		return;
	}
	File *file = source_file_by_id(location.file_id);
	if (compiler.build.test_output || compiler.build.benchmark_output)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("Error|%s|%d|%s\n", file->name, location.row, message);
				return;
			case PRINT_TYPE_NOTE:
				// Note should not be passed on.
				return;
			case PRINT_TYPE_WARN:
				eprintf("Warning|%s|%d|%s\n", file->name, location.row, message);
				return;
			default:
				UNREACHABLE
		}
	}
	unsigned max_line_length = (unsigned)round(log10(location.row)) + 1;
	unsigned max_lines_for_display = MAX_WIDTH - max_line_length - 2;
	char number_buffer[20];
	char number_buffer_elided[20];
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);
	snprintf(number_buffer_elided, 20, "%%%dd: %%.*s|\n", max_line_length);
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);
	snprintf(number_buffer_elided, 20, "%%%dd: %%.*s|\n", max_line_length);

	// Insert end in case it's not yet there.

	const char *file_contents = file->contents;
	int64_t display_row = location.row;
	int64_t row_start = display_row - LINES_SHOWN + 1;
	if (row_start < 1) row_start = 1;
	int64_t row = 1;
	const char *current = file_contents;
	// Progress to the first row.
	while (row < row_start)
	{
		if (current++[0] == '\n')
		{
			row++;
		}
	}
	int row_len = -1;
	while (row <= display_row)
	{
		current += row_len + 1;
		row_len = 0;
		while (current[row_len] != '\n' && current[row_len]) row_len++;
		if (row_len > max_lines_for_display)
		{
			eprintf(number_buffer_elided, row, max_lines_for_display - 1, current);
		}
		else
		{
			eprintf(number_buffer, row, row_len, current);
		}
		row++;
	}
	eprintf("  ");
	for (unsigned i = 0; i < max_line_length; i++)
	{
		eprintf(" ");
	}
	unsigned col_location = location.col;
	if (!col_location || col_location > max_lines_for_display) col_location = 0;
	unsigned space_to = col_location ? col_location : max_lines_for_display - 1;
	for (unsigned i = 0; i < space_to - 1; i++)
	{
		switch (current[i])
		{
			case '\t':
				eprintf("\t");
				break;
			default:
				eprintf(" ");
				break;
		}
	}
	unsigned len = location.length;
	if (!len) len = 1;
	if (col_location)
	{
		for (uint32_t i = 0; i < len; i++)
		{
			eprintf("^");
		}
	}
	eprintf("\n");

	if (col_location)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("(%s:%d:%d) Error: %s\n\n", file->full_path, location.row, col_location, message);
				break;
			case PRINT_TYPE_NOTE:
				eprintf("(%s:%d:%d) Note: %s\n\n", file->full_path, location.row, col_location, message);
				break;
			case PRINT_TYPE_WARN:
				eprintf("(%s:%d:%d) Warning: %s\n\n", file->full_path, location.row, col_location, message);
				break;
			default:
				UNREACHABLE
		}
	}
	else
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("(%s:%d) Error: %s\n\n", file->full_path, location.row, message);
				break;
			case PRINT_TYPE_NOTE:
				eprintf("(%s:%d) Note: %s\n\n", file->full_path, location.row, message);
				break;
			case PRINT_TYPE_WARN:
				eprintf("(%s:%d) Warning: %s\n\n", file->full_path, location.row, message);
				break;
			default:
				UNREACHABLE
		}

	}

}

static void vprint_error(SourceSpan location, const char *message, va_list args)
{
	print_error_type_at(location, str_vprintf(message, args), PRINT_TYPE_ERROR);
}


void sema_verror_range(SourceSpan location, const char *message, va_list args)
{
	vprint_error(location, message, args);
	compiler.context.errors_found++;
}

void sema_warning_at(SourceSpan loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	print_error_type_at(loc, str_vprintf(message, list), PRINT_TYPE_NOTE);
	va_end(list);
}


void print_error_at(SourceSpan loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_range(loc, message, list);
	va_end(list);
}

void print_error_after(SourceSpan loc, const char *message, ...)
{
	loc.col += loc.length;
	loc.length = 1;
	va_list list;
	va_start(list, message);
	sema_verror_range(loc, message, list);
	va_end(list);
}

void sema_error_prev_at(SourceSpan loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
#define MAX_ERROR_LEN 4096
	char buffer[MAX_ERROR_LEN];
	size_t written = vsnprintf(buffer, MAX_ERROR_LEN - 1, message, args);
	// Ignore errors
	if (written <= MAX_ERROR_LEN - 2)
	{
		print_error_type_at(loc, buffer, PRINT_TYPE_NOTE);
	}
	va_end(args);
	return;
}


void print_error(ParseContext *context, const char *message, ...)
{
	compiler.context.errors_found++;
	File *file = context->unit->file;
	va_list list;
	va_start(list, message);
	eprintf("(%s:0) Error: ", file->name);
	evprintf(message, list);
	eprintf("\n");
	va_end(list);
}

// This function is fairly slow, which is a reflection on how
// often it is supposed to be used.
void span_to_scratch(SourceSpan span)
{
	File *file = source_file_by_id(span.file_id);
	const char *current = file->contents;
	uint32_t row = 1;
	uint32_t row_to_find = span.row;
	uint32_t length = span.length;
	uint32_t col = span.col;
	if (!row_to_find || !length || !col) return;
	while (row < row_to_find)
	{
		switch (current++[0])
		{
			case '\0':
				return;
			case '\n':
				row++;
			default:
				break;
		}
	}
	assert(row == row_to_find);
	const char *start = current + col - 1;
	bool last_was_whitespace = false;
	for (uint32_t i = 0; i < length; i++)
	{
		char c = start[i];
		if (char_is_whitespace(c))
		{
			if (!last_was_whitespace) scratch_buffer_append_char(' ');
			last_was_whitespace = true;
			continue;
		}
		last_was_whitespace = false;
		scratch_buffer_append_char(c);
	}
}

// This function is fairly slow, which is a reflection on how
// often it is supposed to be used.
const char *span_to_string(SourceSpan span)
{
	File *file = source_file_by_id(span.file_id);
	const char *current = file->contents;
	uint32_t row = 1;
	uint32_t row_to_find = span.row;
	uint32_t length = span.length;
	uint32_t col = span.col;
	if (!row_to_find || !length || !col) return NULL;
	while (row < row_to_find)
	{
		switch (current++[0])
		{
			case '\0':
				return NULL;
			case '\n':
				row++;
			default:
				break;
		}
	}
	assert(row == row_to_find);
	const char *start = current + col - 1;
	return str_copy(start, length);
}
