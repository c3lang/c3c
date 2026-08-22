// Copyright (c) 2019-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <math.h>

#define LINES_SHOWN 4
#define MAX_WIDTH 120
#define MAX_ERROR_LEN 4096
#define TAB_WIDTH 4
#define SHIFT_PADDING TAB_WIDTH * 5

#define TAB_REM(column) (TAB_WIDTH - ((column) % TAB_WIDTH))

static void eprint_escaped_string(const char *message)
{
	(void)fputc('"', stderr);
	char c;
	while ((c = *(message++)) != 0)
	{
		switch (c)
		{
			case '\t':
				(void)fputs("\\t", stderr);
				break;
			case '\r':
				break;
			case '|':
				(void)fputs("\\x7c", stderr);
				break;
			case '\"':
				(void)fputs("\\\"", stderr);
				break;
			case '\\':
				(void)fputs("\\\\", stderr);
				break;
			case '\n':
				(void)fputs("\\n", stderr);
				break;
			default:
				(void)fputc(c, stderr);
		}
	}
	(void)fputc('"', stderr);
}

static int utf8_seq_len(unsigned char c)
{
	if (c < 0x80) return 1;
	if (c < 0xE0) return 2;
	if (c < 0xF0) return 3;
	return 4;
}

static void width_of(char c, unsigned column, unsigned *w, unsigned *l)
{
	if (c == '\t')
	{
		*w = TAB_REM(column);
		*l = 1;
		return;
	}

	*l = utf8_seq_len(c);
	//! There are unicode chars that have a visual width of 2
	*w = 1;
}

static int render_line(char *dst, const unsigned vlen, const char *src)
{
	unsigned len = 0;
	unsigned width = 0;

	unsigned w = 0;
	unsigned l = 0;
	while (width < vlen)
	{
		width_of(*src, width, &w, &l);
		
		if (*src == '\t')
		{
			memset(dst + len, ' ', w);
			len += w;
		}
		else
		{
			memcpy(dst + len, src, l);
			len += l;
		}
		width += w;
		src += l;
	}
	return (int)len;
}

static void print_error_type_at(SourceLoc *location, const char *message, PrintType print_type)
{
	if (!location)
	{
		eprintf("Unlocalized error: %s.\n", message);
		return;
	}
	File *file = source_file_by_id(location->file_id);
	if (compiler.build.lsp_output)
	{
		eprintf("> LSPERR|");
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("error");
				break;
			case PRINT_TYPE_NOTE:
				eprintf("note");
				break;
			case PRINT_TYPE_WARN:
				eprintf("warn");
				break;
			default:
				UNREACHABLE_VOID
		}
		eprintf("|");
		eprint_escaped_string(file->full_path);
		eprintf("|%d|%d|", location->row, location->col);
		eprint_escaped_string(message);
		eprintf("\n");
		return;
	}
	else if (compiler.build.test_output || compiler.build.benchmark_output)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("Error|%s|%d|%d|%s\n", file->full_path, location->row, location->col, message);
				return;
			case PRINT_TYPE_NOTE:
				// Note should not be passed on.
				return;
			case PRINT_TYPE_WARN:
				eprintf("Warning|%s|%d|%d|%s\n", file->full_path, location->row, location->col, message);
				return;
			default:
				UNREACHABLE_VOID
		}
	}

	// Fit a line of full unicode characters
	char render_buffer[MAX_WIDTH * 4];
	
	unsigned row_prefix_width = (unsigned)floor(log10(location->row)) + 1;
	char prefix_buffer[16];
	snprintf(prefix_buffer, 16, " %%%dd: ", row_prefix_width);
	row_prefix_width++; // add ':'

	const char *elipsis = "...";
	const unsigned elipsis_len = 4; // with space

	const unsigned padded_spaces = 2;
	unsigned display_line_width = MAX_WIDTH - row_prefix_width - padded_spaces;

	// Insert end in case it's not yet there.

	const char *file_contents = file->contents;
	int64_t display_row = location->row;
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

	const char *loc_pos = file->contents + location->offset;
	const char *loc_end = file->contents + location->offset + location->length;
	unsigned column = location->col;	

	const char *row_offset = current;
	unsigned row_width = 0;
	bool is_elided = false;
	bool needs_shift = column > display_line_width;

	while (row <= display_row)
	{
		const bool is_last_row = row == display_row;
		current = row_offset;

		if (needs_shift && is_last_row)
		{
			current = loc_pos;
			for (int i = SHIFT_PADDING; i > 0; --i)
			{
				do { current--; }
				while ((*current & 0xC0) == 0x80);
			}
			row_offset = current;
		}

		row_width = 0;
		while (*row_offset != '\n' && *row_offset)
		{
			unsigned w, l;
			width_of(*row_offset, row_width, &w, &l);
			row_width += w;
			row_offset += l;
		}

		unsigned render_line_width = display_line_width;
		if (needs_shift && is_last_row) render_line_width -= elipsis_len;		
		is_elided = row_width > render_line_width;
 		render_line_width = !is_elided ? row_width : render_line_width - elipsis_len;

		char *render_offset = render_buffer;
		render_offset += sprintf(render_offset, prefix_buffer, (int)row);
		if (needs_shift && is_last_row)
		{
			render_offset += sprintf(render_offset, "%s ", elipsis);
		}
		render_offset += render_line(render_offset, render_line_width, current);
		if (is_elided)
		{
			render_offset += sprintf(render_offset, " %s", elipsis);
		}
		eprintf("%.*s\n", render_offset - render_buffer, render_buffer);

		row++;
		row_offset++; // skip '\n'
	}

	unsigned prefix_width = row_prefix_width + padded_spaces;
	if (needs_shift) prefix_width += elipsis_len;
	display_line_width = MAX_WIDTH - prefix_width;
	
	eprintf("%*s", prefix_width, "");

	unsigned space_width = 0;
	while (current < loc_pos)
	{
		unsigned w, l;
		width_of(*current, space_width, &w, &l);
		space_width += w;
		current += l;		
		eprintf("%*s", w, "");
	}
	
	const bool is_multiline = (ptrdiff_t)location->length > (row_offset - current) && row_width < display_line_width;
	unsigned highlighter_width = is_elided ? display_line_width  : row_width;
	highlighter_width -= space_width;
	
	// The char at the start of a loc is currently, always ascii
	eprintf("^");
	current++;
	
	unsigned i = 1;
	while (current < loc_end && i < highlighter_width)
	{
		unsigned w, l;
		width_of(*current, i, &w, &l);
		i += w;
		current += l;
		while (w-- > 0) eprintf(is_elided ? "~" : "^");
	}

	if (is_multiline)
	{
		unsigned rows_left = 1;
		while (row_offset++ < loc_end)
		{
			if (*row_offset == '\n') rows_left++;
		}
		eprintf(" (+%u lines)", rows_left);
	}
	eprintf("\n");

	bool ansi = use_ansi();
	if (column)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				if (ansi)
				{
					eprintf("%s:%d:%d: \x1b[31;1merror\x1b[0m: %s\n\n", file->full_path, location->row, column, message);
				}
				else 
				{
					eprintf("%s:%d:%d: error: %s\n\n", file->full_path, location->row, column, message);
				}
				break;
			case PRINT_TYPE_NOTE:
				if (ansi)
				{
					eprintf("%s:%d:%d: \x1b[1mnote\x1b[0m: %s\n\n", file->full_path, location->row, column, message);
				}
				else
				{
					eprintf("%s:%d:%d: note: %s\n\n", file->full_path, location->row, column, message);
				}
				break;
			case PRINT_TYPE_WARN:
				if (ansi)
				{
					eprintf("%s:%d:%d: \x1b[33;1mwarning\x1b[0m: %s\n\n", file->full_path, location->row, column, message);
				}
				else 
				{
					eprintf("%s:%d:%d: warning: %s\n\n", file->full_path, location->row, column, message);
				}
				break;
			default:
				UNREACHABLE_VOID
		}
	}
	else
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				if (ansi)
				{
					eprintf("%s:%d: \x1b[31;1merror\x1b[0m: %s\n\n", file->full_path, location->row, message);
				}
				else 
				{
					eprintf("%s:%d: error: %s\n\n", file->full_path, location->row, message);
				}
				break;
			case PRINT_TYPE_NOTE:
				if (ansi) 
				{
					eprintf("%s:%d: \x1b[1mnote\x1b[0m: %s\n\n", file->full_path, location->row, message);
				} 
				else
				{
					eprintf("%s:%d: note: %s\n\n", file->full_path, location->row, message);
				}
				break;
			case PRINT_TYPE_WARN:
				if (ansi)
				{
					eprintf("%s:%d: \x1b[33;1mwarning\x1b[0m: %s\n\n", file->full_path, location->row, message);
				}
				else 
				{
					eprintf("%s:%d: warning: %s\n\n", file->full_path, location->row, message);
				}
				break;
			default:
				UNREACHABLE_VOID
		}

	}

}

static void vprint_msg(SourceLoc *location, const char *message, va_list args, PrintType type)
{
	print_error_type_at(location, str_vprintf(message, args), type);
}


void sema_verror_range(SourceLoc *location, const char *message, va_list args)
{
	vprint_msg(location, message, args, PRINT_TYPE_ERROR);
	compiler.context.errors_found++;
}

void sema_vwarn_range(SourceLocId location, const char *message, va_list args)
{
	vprint_msg(sourcelocptrzero(location), message, args, PRINT_TYPE_WARN);
}

void sema_warning_at(SourceLocId loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	print_error_type_at(sourcelocptrzero(loc), str_vprintf(message, list), PRINT_TYPE_WARN);
	va_end(list);
}

void print_error_at_loc(SourceLoc *loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_range(loc, message, list);
	va_end(list);
}

void print_error_at(SourceLocId loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_range(sourcelocptrzero(loc), message, list);
	va_end(list);
}


void print_error_after(SourceLoc *curr, const char *message, ...)
{
	SourceLoc loc = *curr;
	loc.col += loc.length;
	loc.length = 1;
	va_list list;
	va_start(list, message);
	sema_verror_range(&loc, message, list);
	va_end(list);
}

void sema_note_prev_at(SourceLocId loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[MAX_ERROR_LEN];
	size_t written = vsnprintf(buffer, MAX_ERROR_LEN - 1, message, args);
	// Ignore errors
	if (written <= MAX_ERROR_LEN - 2)
	{
		print_error_type_at(sourcelocptrzero(loc), buffer, PRINT_TYPE_NOTE);
	}
	va_end(args);
}

void print_deprecation_at(SourceLocId loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[MAX_ERROR_LEN];
	size_t written = vsnprintf(buffer, MAX_ERROR_LEN - 1, message, args);
	// Ignore errors
	if (written <= MAX_ERROR_LEN - 2)
	{
		print_error_type_at(sourcelocptrzero(loc), buffer, compiler.build.warnings.deprecation == WARNING_WARN ? PRINT_TYPE_NOTE : PRINT_TYPE_ERROR);
	}
	static bool deprecation_hint = false;
	if (!compiler.build.lsp_output && !deprecation_hint)
	{
		deprecation_hint = true;
		eprintf("HINT: You may use --warn-deprecation=no to silence deprecation warnings.\n\n");
	}
	va_end(args);
}

void print_deprecation_at_loc(SourceLoc *loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[MAX_ERROR_LEN];
	size_t written = vsnprintf(buffer, MAX_ERROR_LEN - 1, message, args);
	// Ignore errors
	if (written <= MAX_ERROR_LEN - 2)
	{
		print_error_type_at(loc, buffer, compiler.build.warnings.deprecation == WARNING_WARN ? PRINT_TYPE_NOTE : PRINT_TYPE_ERROR);
	}
	static bool deprecation_hint = false;
	if (!compiler.build.lsp_output && !deprecation_hint)
	{
		deprecation_hint = true;
		eprintf("HINT: You may use --warn-deprecation=no to silence deprecation warnings.\n\n");
	}
	va_end(args);
}


void sema_warn_prev_at(SourceLocId loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[MAX_ERROR_LEN];
	size_t written = vsnprintf(buffer, MAX_ERROR_LEN - 1, message, args);
	// Ignore errors
	if (written <= MAX_ERROR_LEN - 2)
	{
		print_error_type_at(sourcelocptrzero(loc), buffer, PRINT_TYPE_WARN);
	}
	va_end(args);
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
void loc_to_scratch(SourceLocId loc)
{
	if (!loc)
	{
		scratch_buffer_append("<unknown code>");
		return;
	}
	SourceLoc *location = sourcelocptr(loc);
	File *file = source_file_by_id(location->file_id);
	const char *current = file->contents + location->offset;
	bool last_was_whitespace = false;
	for (uint32_t i = 0; i < location->length; i++)
	{
		char c = current[i];
		// This does not properly handle strings
		if (c <= ' ')
		{
			if (last_was_whitespace) continue;
			last_was_whitespace = true;
			scratch_buffer_append_char(' ');
			continue;
		}
		last_was_whitespace = false;
		scratch_buffer_append_char(c);
	}
}

// This function is fairly slow, which is a reflection on how
// often it is supposed to be used.
const char *span_to_string(SourceLocId span)
{
	SourceLoc *location = sourcelocptr(span);
	File *file = source_file_by_id(location->file_id);
	return str_copy(file->contents + location->offset, location->length);
}

void assert_print_line(SourceLocId loc)
{
	if (loc == 0)
	{
		eprintf("Assert analysing code at unknown location:\n");
		return;
	}
	SourceLoc *location = sourcelocptr(loc);
	File *file = source_file_by_id(location->file_id);
	eprintf("Assert analysing '%s' at row %d, col %d.\n", file->name, location->row, location->col);
}
