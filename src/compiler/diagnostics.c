// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "compiler_internal.h"
#include <math.h>

Diagnostics diagnostics;

void diag_reset(void)
{
	diagnostics.panic_mode = false;
	diagnostics.errors = 0;
	diagnostics.warnings = 0;
}

void reset_panic_mode(void)
{
	diagnostics.panic_mode = false;
}

typedef enum
{
	PRINT_TYPE_ERROR,
	PRINT_TYPE_PREV,
	PRINT_TYPE_WARN
} PrintType;

static void print_error(SourceRange source_range, const char *message, PrintType print_type)
{
	SourcePosition position = source_file_find_position(source_range.loc);

	static const int LINES_SHOWN = 4;

	unsigned max_line_length = (int)round(log10(position.line)) + 1;

	char number_buffer[20];
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

	// Insert end in case it's not yet there.
	for (SourceLoc s = position.loc; s < position.file->end_id; s++)
	{
		if ((position.file->contents + s - position.file->start_id)[0] == '\n')
		{
			source_file_append_line_end(position.file, s);
			break;
		}
	}
	size_t lines_in_file = vec_size(position.file->lines);
	for (unsigned i = LINES_SHOWN; i > 0; i--)
	{
		if (position.line < i) continue;
		uint32_t line_number = position.line + 1 - i;
		SourceLoc line_start = position.file->lines[line_number - 1];

		SourceLoc line_end = line_number == lines_in_file ? position.file->end_id :
				position.file->lines[line_number];
		uint32_t line_len = line_end - line_start - 1;
 		eprintf(number_buffer, line_number, line_len, position.file->contents + line_start - position.file->start_id);
	}
	eprintf("  ");
	for (unsigned i = 0; i < max_line_length; i++)
	{
		eprintf(" ");
	}
	for (unsigned i = 0; i < position.col - 1; i++)
	{
		if (position.start[i] == '\t')
		{
			eprintf("\t");
		}
		else
		{
			eprintf(" ");
		}
	}
	for (uint32_t i = 0; i < source_range_len(source_range); i++)
	{
		eprintf("^");
	}
	eprintf("\n");

	switch (print_type)
	{
		case PRINT_TYPE_ERROR:
			eprintf("(%s:%d) Error: %s\n", position.file->name, position.line, message);
			break;
		case PRINT_TYPE_PREV:
			eprintf("(%s:%d) %s\n", position.file->name, position.line, message);
			break;
		case PRINT_TYPE_WARN:
			eprintf("(%s:%d) Warning: %s\n", position.file->name, position.line, message);
			break;
		default:
			UNREACHABLE
	}

}


static void vprint_error(SourceRange span, const char *message, va_list args)
{
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error(span, buffer, PRINT_TYPE_ERROR);
}

void diag_error_range(SourceRange span, const char *message, ...)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	va_list args;
	va_start(args, message);
	vprint_error(span, message, args);
	va_end(args);
	diagnostics.errors++;
}

void diag_verror_range(SourceRange span, const char *message, va_list args)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	vprint_error(span, message, args);
	diagnostics.errors++;
}

void sema_error_at(SourceLoc loc, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_at(loc, message, list);
	va_end(list);
}

void sema_error_range(SourceRange range, const char *message, ...)
{
	va_list list;
	va_start(list, message);
	sema_verror_range(range, message, list);
	va_end(list);
}

void sema_verror_at(SourceLoc loc, const char *message, va_list args)
{
	vprint_error((SourceRange) { loc, loc + 1 }, message, args);
	diagnostics.errors++;
}

void sema_verror_range(SourceRange range, const char *message, va_list args)
{
	vprint_error(range, message, args);
	diagnostics.errors++;
}

void sema_error(Context *context, const char *message, ...)
{
	File *file = lexer_current_file(&context->lexer);
	va_list list;
	va_start(list, message);
	eprintf("(%s:0) Error: ", file->name);
	evprintf(message, list);
	eprintf("\n");
	va_end(list);
}

void sema_prev_at_range(SourceRange span, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error(span, buffer, PRINT_TYPE_PREV);
	va_end(args);
}

void sema_prev_at(SourceLoc loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error((SourceRange){ loc, loc + 1 }, buffer, PRINT_TYPE_PREV);
	va_end(args);
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






void prev_at_range(SourceRange span, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error(span, buffer, PRINT_TYPE_PREV);
	va_end(args);
}

void prev_at(SourceLoc loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	char buffer[256];
	vsnprintf(buffer, 256, message, args);
	print_error((SourceRange){ loc, 1 }, buffer, PRINT_TYPE_PREV);
	va_end(args);
}

void sema_error_range(SourceRange token, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	vprint_error(token, message, args);
	va_end(args);
	diagnostics.errors++;
}

void sema_error_at(SourceLoc loc, const char *message, ...)
{
	va_list args;
	va_start(args, message);
	vprint_error((SourceRange) { loc, 1 }, message, args);
	va_end(args);
	diagnostics.errors++;
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