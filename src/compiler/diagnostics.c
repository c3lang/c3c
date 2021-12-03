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

static void print_error2(SourceLocation *location, const char *message, PrintType print_type)
{
	if (active_target.test_output)
	{
		switch (print_type)
		{
			case PRINT_TYPE_ERROR:
				eprintf("Error|%s|%d|%s\n", location->file->name, location->line, message);
				return;
			case PRINT_TYPE_PREV:
				return;
			case PRINT_TYPE_WARN:
				eprintf("Warning|%s|%d|%s\n", location->file->name, location->line, message);
				return;
			default:
				UNREACHABLE
		}
	}
	static const int LINES_SHOWN = 4;

	unsigned max_line_length = (unsigned)round(log10(location->line)) + 1;

	char number_buffer[20];
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

	// Insert end in case it's not yet there.
	for (SourceLoc s = location->start; s < location->file->end_id; s++)
	{
		if ((location->file->contents + s - location->file->start_id)[0] == '\n')
		{
			source_file_append_line_end(location->file, s);
			break;
		}
	}
	size_t lines_in_file = vec_size(location->file->lines);
	const char *start = NULL;
	for (unsigned i = LINES_SHOWN; i > 0; i--)
	{
		if (location->line < i) continue;
		uint32_t line_number = location->line + 1 - i;
		SourceLoc line_start = location->file->lines[line_number - 1];

		SourceLoc line_end = line_number == lines_in_file ? location->file->end_id + 1 :
		                     location->file->lines[line_number];
		uint32_t line_len = line_end - line_start - 1;
		start = location->file->contents + line_start - location->file->start_id;
		eprintf(number_buffer, line_number, line_len, start);
	}
	eprintf("  ");
	for (unsigned i = 0; i < max_line_length; i++)
	{
		eprintf(" ");
	}

	for (unsigned i = 1; i < location->col; i++)
	{
		switch (start[i])
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
			eprintf("(%s:%d) Error: %s\n\n", location->file->name, location->line, message);
			break;
		case PRINT_TYPE_PREV:
			eprintf("(%s:%d) %s\n\n", location->file->name, location->line, message);
			break;
		case PRINT_TYPE_WARN:
			eprintf("(%s:%d) Warning: %s\n\n", location->file->name, location->line, message);
			break;
		default:
			UNREACHABLE
	}

}

static void vprint_error(SourceLocation *location, const char *message, va_list args)
{
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error2(location, buffer, PRINT_TYPE_ERROR);
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
	print_error2(&loc, buffer, PRINT_TYPE_PREV);
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
	if (curr->file != prev->file)
	{
		// Ok, this is the first location, so then we create a "start" location:
		location = *curr;
		location.start = 0;
		location.line = 1;
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
	File *file = lexer_current_file(context->lexer);
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