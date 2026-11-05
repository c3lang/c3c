// Copyright (c) 2026 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the GNU LGPLv3.0 license
// a copy of which can be found in the LICENSE file.

#include "lib.h"

// Tables for the compiler statistics. A table is " [#]  name  col col col [bar]",
// where the header sets the column widths and the units go in the header.
// A NULL header hides the column, the rows still pass a value for it.
#define STATS_WIDTH 104
#define STATS_MIN_COLUMN_WIDTH 11
#define STATS_BAR_WIDTH 40
#define STATS_MAX_COLUMNS 8
#define STATS_BUFFERS 32
#define STATS_BUFFER_SIZE 64

#define ANSI_BOLD "\x1b[1m"
#define ANSI_DIM "\x1b[2m"
#define ANSI_BAR "\x1b[36m"
#define ANSI_RESET "\x1b[0m"

static char stats_buffers[STATS_BUFFERS][STATS_BUFFER_SIZE];
static int stats_buffer_index;
static bool stats_colour;
static bool stats_indexed;
static int stats_name_width;
static int stats_column_widths[STATS_MAX_COLUMNS];
static bool stats_column_hidden[STATS_MAX_COLUMNS];
static int stats_columns;

static char *stats_buffer(void)
{
	stats_buffer_index = (stats_buffer_index + 1) % STATS_BUFFERS;
	return stats_buffers[stats_buffer_index];
}

static void stats_style(const char *style)
{
	if (stats_colour) fputs(style, stdout);
}

static void stats_repeat(const char *str, int count)
{
	for (int i = 0; i < count; i++) fputs(str, stdout);
}

static void stats_rule(const char *line)
{
	stats_style(ANSI_DIM);
	stats_repeat(line, STATS_WIDTH);
	stats_style(ANSI_RESET);
	putchar('\n');
}

// The display width, counting UTF-8 code points rather than bytes.
static int stats_display_width(const char *str)
{
	int width = 0;
	for (; *str; str++)
	{
		if ((*str & 0xC0) != 0x80) width++;
	}
	return width;
}

// Print at most `width` code points, cutting with "..." and padding to the width.
static void stats_print_padded(const char *str, int width, bool right_align)
{
	int str_width = stats_display_width(str);
	if (str_width > width)
	{
		int keep = width - 3;
		const char *end = str;
		for (int count = 0; *end; end++)
		{
			if ((*end & 0xC0) != 0x80 && count++ == keep) break;
		}
		printf("%.*s...", (int)(end - str), str);
		return;
	}
	if (right_align) stats_repeat(" ", width - str_width);
	fputs(str, stdout);
	if (!right_align) stats_repeat(" ", width - str_width);
}

static int stats_table_width(void)
{
	int width = stats_name_width + (stats_indexed ? 6 : 0);
	for (int i = 0; i < stats_columns; i++)
	{
		if (!stats_column_hidden[i]) width += stats_column_widths[i];
	}
	return width;
}

// Print a bar of STATS_BAR_WIDTH cells for 1.0, with eighths of a cell for the remainder.
static void stats_bar(double fraction)
{
	static const char *eighths[8] = { "", "▏", "▎", "▍", "▌", "▋", "▊", "▉" };
	if (fraction < 0) fraction = 0;
	if (fraction > 1) fraction = 1;
	int total = (int)(fraction * STATS_BAR_WIDTH * 8 + 0.5);
	putchar(' ');
	stats_style(ANSI_BAR);
	for (int i = 0; i < total / 8; i++) fputs("█", stdout);
	fputs(eighths[total % 8], stdout);
	// Something rather than nothing for tiny values.
	if (!total && fraction > 0) fputs(eighths[1], stdout);
	stats_style(ANSI_RESET);
}

static void stats_vrow(const char *index, const char *name, double bar, int columns, va_list args)
{
	if (stats_indexed)
	{
		stats_style(ANSI_DIM);
		printf(" %3s  ", index);
		stats_style(ANSI_RESET);
	}
	else
	{
		putchar(' ');
	}
	stats_print_padded(name, stats_name_width, false);
	for (int i = 0; i < columns; i++)
	{
		const char *value = va_arg(args, const char *);
		if (i < stats_columns && stats_column_hidden[i]) continue;
		int width = i < stats_columns ? stats_column_widths[i] : STATS_MIN_COLUMN_WIDTH;
		stats_print_padded(value ? value : "", width, true);
	}
	if (bar >= 0) stats_bar(bar);
	putchar('\n');
}

// Insert thousands separators in the integer part of a formatted number.
static const char *stats_group_digits(const char *number)
{
	char *buffer = stats_buffer();
	int out = 0;
	if (*number == '-') buffer[out++] = *number++;
	int digits = (int)strspn(number, "0123456789");
	for (int i = 0; i < digits; i++)
	{
		if (i && (digits - i) % 3 == 0) buffer[out++] = ',';
		buffer[out++] = number[i];
	}
	snprintf(buffer + out, STATS_BUFFER_SIZE - out, "%s", number + digits);
	return buffer;
}

void stats_set_colour(bool colour)
{
	stats_colour = colour;
}

void stats_banner(const char *title)
{
	stats_rule("═");
	putchar(' ');
	stats_style(ANSI_BOLD);
	fputs(title, stdout);
	stats_style(ANSI_RESET);
	putchar('\n');
	stats_rule("═");
}

void stats_section(const char *title)
{
	putchar('\n');
	stats_style(ANSI_DIM);
	fputs("━━ ", stdout);
	stats_style(ANSI_RESET);
	stats_style(ANSI_BOLD);
	fputs(title, stdout);
	stats_style(ANSI_RESET);
	stats_style(ANSI_DIM);
	putchar(' ');
	stats_repeat("━", STATS_WIDTH - 4 - stats_display_width(title));
	stats_style(ANSI_RESET);
	putchar('\n');
}

void stats_table(const char *caption, bool indexed, int name_width)
{
	putchar('\n');
	if (caption)
	{
		putchar(' ');
		stats_style(ANSI_BOLD);
		fputs(caption, stdout);
		stats_style(ANSI_RESET);
		puts("\n");
	}
	stats_indexed = indexed;
	stats_name_width = name_width;
	stats_columns = 0;
}

void stats_header(const char *name, int columns, ...)
{
	va_list args;
	va_start(args, columns);
	if (columns > STATS_MAX_COLUMNS) columns = STATS_MAX_COLUMNS;
	va_list widths;
	va_copy(widths, args);
	for (int i = 0; i < columns; i++)
	{
		const char *header = va_arg(widths, const char *);
		stats_column_hidden[i] = !header;
		int width = header ? stats_display_width(header) + 2 : 0;
		stats_column_widths[i] = width > STATS_MIN_COLUMN_WIDTH ? width : STATS_MIN_COLUMN_WIDTH;
	}
	va_end(widths);
	stats_columns = columns;
	stats_style(ANSI_BOLD);
	stats_vrow("#", name, -1, columns, args);
	stats_style(ANSI_RESET);
	va_end(args);
	stats_table_rule();
}

void stats_table_rule(void)
{
	putchar(' ');
	stats_style(ANSI_DIM);
	stats_repeat("─", stats_table_width());
	stats_style(ANSI_RESET);
	putchar('\n');
}

void stats_row(int index, const char *name, int columns, ...)
{
	char index_str[16] = "";
	if (index > 0) snprintf(index_str, sizeof(index_str), "%d", index);
	va_list args;
	va_start(args, columns);
	stats_vrow(index_str, name, -1, columns, args);
	va_end(args);
}

void stats_bar_row(const char *name, double bar, int columns, ...)
{
	va_list args;
	va_start(args, columns);
	stats_vrow("", name, bar, columns, args);
	va_end(args);
}

void stats_total_row(const char *name, int columns, ...)
{
	va_list args;
	va_start(args, columns);
	stats_style(ANSI_BOLD);
	stats_vrow("", name, -1, columns, args);
	stats_style(ANSI_RESET);
	va_end(args);
}

void stats_note(const char *note)
{
	putchar(' ');
	stats_style(ANSI_DIM);
	fputs(note, stdout);
	stats_style(ANSI_RESET);
	putchar('\n');
}

const char *stats_fmt(const char *format, ...)
{
	char *buffer = stats_buffer();
	va_list args;
	va_start(args, format);
	vsnprintf(buffer, STATS_BUFFER_SIZE, format, args);
	va_end(args);
	return buffer;
}

const char *stats_int(int64_t value)
{
	return stats_group_digits(stats_fmt("%lld", (long long)value));
}

const char *stats_kb(int64_t bytes)
{
	return stats_int(bytes / 1024);
}

const char *stats_ms(double seconds)
{
	return stats_group_digits(stats_fmt("%.3f", seconds * 1000));
}

const char *stats_percent(double fraction)
{
	return stats_fmt("%.1f", fraction * 100);
}

const char *stats_duration(double seconds)
{
	if (seconds >= 1) return stats_fmt("%.3f s", seconds);
	return stats_fmt("%.1f ms", seconds * 1000);
}
