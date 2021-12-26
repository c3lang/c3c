// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <math.h>



typedef enum
{
	PRINT_TYPE_ERROR,
	PRINT_TYPE_PREV,
	PRINT_TYPE_WARN
} PrintType;

#define LINES_SHOWN 4

static void print_error(SourceLocation *location, const char *message, PrintType print_type)
{
	File *file = source_file_by_id(location->file_id);
	if (active_target.test_output)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("Error|%s|%d|%s\n", file->name, location->row, message);
				return;
			case PRINT_TYPE_PREV:
				return;
			case PRINT_TYPE_WARN:
				eprintf("Warning|%s|%d|%s\n", file->name, location->row, message);
				return;
			default:
				UNREACHABLE
		}
	}
	unsigned max_line_length = (unsigned)round(log10(location->row)) + 1;

	char number_buffer[20];
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

	// Insert end in case it's not yet there.

	const char *file_contents = file->contents;
	int lines_found = 0;
	size_t line_starts[LINES_SHOWN + 1] = { 0, 0, 0, 0 };
	uint32_t start = location->start;
	if (start < 2)
	{
		line_starts[++lines_found] = 0;
	}
	else
	{
		for (size_t i = start; i > 0; i--)
		{
			if (file_contents[i - 1] == '\n')
			{
				line_starts[++lines_found] = i;
				if (lines_found >= LINES_SHOWN) break;
			}
			if (i == 1)
			{
				line_starts[++lines_found] = 0;
				break;
			}
		}
	}
	for (size_t i = start; ; i++)
	{
		switch (file_contents[i])
		{
			case '\0':
			case '\n':
				line_starts[0] = i + 1;
				goto FOUND;
			default:
				continue;
		}
	}
	FOUND:;
	const char *start_char = NULL;
	for (unsigned i = lines_found; i > 0; i--)
	{
		SourceLoc line_start = line_starts[i];
		SourceLoc line_end = line_starts[i - 1] - 1;
		uint32_t line_number = location->row + 1 - i;
		uint32_t line_len = line_end - line_start;
		start_char = file->contents + line_start;
		eprintf(number_buffer, line_number, line_len, start_char);
	}
	eprintf("  ");
	for (unsigned i = 0; i < max_line_length; i++)
	{
		eprintf(" ");
	}

	for (unsigned i = 1; i < location->col; i++)
	{
		switch (start_char[i])
		{
			case '\t':
				eprintf("\t");
			default:
				eprintf(" ");
		}
	}
	for (uint32_t i = 0; i < location->length; i++)
	{
		eprintf("^");
	}
	eprintf("\n");

	switch (print_type)
	{
		case PRINT_TYPE_ERROR:
			eprintf("(%s:%d) Error: %s\n\n", file->name, location->row, message);
			break;
		case PRINT_TYPE_PREV:
			eprintf("(%s:%d) %s\n\n", file->name, location->row, message);
			break;
		case PRINT_TYPE_WARN:
			eprintf("(%s:%d) Warning: %s\n\n", file->name, location->row, message);
			break;
		default:
			UNREACHABLE
	}

}

static void vprint_error(SourceLocation *location, const char *message, va_list args)
{
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error(location, buffer, PRINT_TYPE_ERROR);
}


void diag_verror_range(SourceLocation *location, const char *message, va_list args)
{
	if (global_context.in_panic_mode) return;
	global_context.in_panic_mode = true;
	vprint_error(location, message, args);
	global_context.errors_found++;
}


void sema_verror_range(SourceLocation *location, const char *message, va_list args)
{
	vprint_error(location, message, args);
	global_context.errors_found++;
}


void sema_prev_at_range3(SourceSpan span, const char *message, ...)
{
	SourceLocation *start = TOKLOC(span.loc);
	SourceLocation *end = TOKLOC(span.end_loc);
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	SourceLocation loc = *start;
	loc.length = end->start - start->start + end->length;
	print_error(&loc, buffer, PRINT_TYPE_PREV);
	va_end(args);
}

void sema_error_range(SourceSpan span, const char *message, ...)
{
	SourceLocation *start = TOKLOC(span.loc);
	SourceLocation *end = TOKLOC(span.end_loc);

	SourceLocation loc = *start;
	loc.length = end->start - start->start + end->length;
	va_list list;
	va_start(list, message);
	sema_verror_range(&loc, message, list);
	va_end(list);
}

void sema_error_at_prev_end(Token token, const char *message, ...)
{
	SourceLocation *curr = TOKLOC(token);
	SourceLocation *prev = TOKLOC((TokenId) { token.id.index - 1 });
	SourceLocation location;
	if (curr->file_id != prev->file_id)
	{
		// Ok, this is the first location, so then we create a "start" location:
		location = *curr;
		location.start = 0;
		location.row = 1;
		location.col = 1;
	}
	else
	{
		// TODO handle multiline
		location = *prev;
		location.col += location.length;
		location.start += location.length;
	}
	location.length = 1;
	va_list list;
	va_start(list, message);
	sema_verror_range(&location, message, list);
	va_end(list);
}


void sema_error(Context *context, const char *message, ...)
{
	global_context.errors_found++;
	File *file = context->file;
	va_list list;
	va_start(list, message);
	eprintf("(%s:0) Error: ", file->name);
	evprintf(message, list);
	eprintf("\n");
	va_end(list);


}



/*



bool diagnostics_silence_warnings(Array *warnings)
{
	for (unsigned i = 0; i < warnings->count; i++)
	{
		const char *warning = warnings->entries[i];
		if (strcmp("no-unused", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED);
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_PARAMETER);
			continue;
		}
		if (strcmp("no-unused-variable", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_VARIABLE);
			continue;
		}
		if (strcmp("no-unused-function", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_FUNCTION);
			continue;
		}
		if (strcmp("no-unused-type", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_TYPE);
			continue;
		}
		if (strcmp("no-unused-module", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_MODULE);
			continue;
		}
		if (strcmp("no-unused-public", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_PUBLIC);
			continue;
		}
		if (strcmp("no-unused-import", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_IMPORT);
			continue;
		}
		if (strcmp("no-unused-label", warning) == 0)
		{
			diagnostics_update_severity(DIAG_IGNORE, DIAG_UNUSED_LABEL);
			continue;
		}
		PRINT_ERROR("recipe has unknown warning: '%s'\n", warning);
		return false;
	}
	return true;
}








void sema_warn_at(DiagnosticsType type, SourceLoc loc, const char *message, ...)
{
	// TODO ENABLE
	return;
	SourceRange span = {.loc = loc, .length = 1};
	switch (diagnostics.severity[type])
	{
		case DIAG_IGNORE:
			return;
		case DIAG_WARN:
			break;
		case DIAG_ERROR:
		{
			va_list args;
			va_start(args, message);
			vprint_error(span, message, args);
			va_end(args);
			diagnostics.errors++;
			return;
		}
	}
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	if (diagnostics.severity[type])
		print_error(span, buffer, PRINT_TYPE_WARN);
	va_end(args);
	diagnostics.warnings++;
}

void sema_warn_range(DiagnosticsType type, SourceRange span, const char *message, ...)
{
	// TODO ENABLE
	return;
	switch (diagnostics.severity[type])
	{
		case DIAG_IGNORE:
			return;
		case DIAG_WARN:
			break;
		case DIAG_ERROR:
		{
			va_list args;
			va_start(args, message);
			vprint_error(span, message, args);
			va_end(args);
			diagnostics.errors++;
			return;
		}
	}
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	if (diagnostics.severity[type]) print_error(span, buffer, PRINT_TYPE_WARN);
	va_end(args);
	diagnostics.warnings++;
}

unsigned errors()
{
	return diagnostics.errors;
}

bool error_found()
{
	return diagnostics.errors > 0;
}
*/