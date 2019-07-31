// Copyright (c) 2019 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "diagnostics.h"
#include "source_file.h"
#include <math.h>
#include <stdarg.h>

typedef struct _Diagnostics
{
	bool panic_mode;
	unsigned errors;
	unsigned warnings;
	bool use_color;
} Diagnostics;

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
	File *file =  source_file_from_position(source_range.loc);

	const char *content = file->contents;
	const char *error_start = file->contents + source_range.loc - file->start_id;

	const static int LINES_SHOWN = 4;

	const char *linestarts[LINES_SHOWN];
	for (int i = 0; i < LINES_SHOWN; i++) linestarts[i] = NULL;
	const char *current = content;
	linestarts[0] = content;
	unsigned line = 1;
	while (current < error_start)
	{
		if (current[0] == '\n')
		{
			line++;
			linestarts[3] = linestarts[2];
			linestarts[2] = linestarts[1];
			linestarts[1] = linestarts[0];
			linestarts[0] = current + 1;
		}
		current++;
	}

	const char *end = NULL;
	while (!end)
	{
		switch (current[0])
		{
			case '\n':
			case '\0':
				end = current;
				break;
			default:
				current++;
				break;
		}
	}

	int max_line_length = (int)round(log10(line)) + 1;

	char number_buffer[20];
	snprintf(number_buffer, 20, "%%%dd: %%.*s\n", max_line_length);

	for (unsigned i = 3; i > 0; i--)
	{
		int line_number = line - i;
		const char *start = linestarts[i];
		if (start == NULL) continue;
		const char *line_end = linestarts[i - 1];
		eprintf(number_buffer, line_number, line_end - start - 1, start);
	}
	eprintf(number_buffer, line, end - linestarts[0], linestarts[0]);
	for (unsigned i = 0; i < max_line_length + 2 + error_start - linestarts[0]; i++)
	{
		eprintf(" ");
	}
	for (int i = 0; i < source_range.length; i++)
	{
		eprintf("^");
	}
	eprintf("\n");

	switch (print_type)
	{
		case PRINT_TYPE_ERROR:
			eprintf("(%s:%d) Error: %s\n", file->name, line, message);
			break;
		case PRINT_TYPE_PREV:
			eprintf("(%s:%d) %s\n", file->name, line, message);
			break;
		case PRINT_TYPE_WARN:
			eprintf("(%s:%d) Warning: %s\n", file->name, line, message);
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

void diag_error_at(SourceRange span, const char *message, ...)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	va_list args;
	va_start(args, message);
	vprint_error(span, message, args);
	va_end(args);
	diagnostics.errors++;
}

void diag_verror_at(SourceRange span, const char *message, va_list args)
{
	if (diagnostics.panic_mode) return;
	diagnostics.panic_mode = true;
	vprint_error(span, message, args);
	diagnostics.errors++;
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