module std::core::string::ansi;
import std::io;

constdef Ansi : inline String
{
	RESET              = "\e[0m",
	BOLD               = "\e[1m",
	DIM                = "\e[2m",
	ITALIC             = "\e[3m",
	UNDERLINE          = "\e[4m",
	BLINK              = "\e[5m",
	BLINK_FAST         = "\e[6m",
	INVERT             = "\e[7m",
	HIDDEN             = "\e[8m",
	STRIKETHROUGH      = "\e[9m",
	DOUBLE_UNDER       = "\e[21m",
	NO_DIM             = "\e[22m",
	NO_ITALIC          = "\e[23m",
	NO_UNDERLINE       = "\e[24m",
	NO_BLINK           = "\e[25m",
	NO_INVERT          = "\e[27m",
	NO_HIDDEN          = "\e[28m",
	NO_STRIKETHROUGH   = "\e[29m",
	BLACK              = "\e[30m",
	RED                = "\e[31m",
	GREEN              = "\e[32m",
	YELLOW             = "\e[33m",
	BLUE               = "\e[34m",
	MAGENTA            = "\e[35m",
	CYAN               = "\e[36m",
	WHITE              = "\e[37m",
	DEFAULT            = "\e[39m",
	BRIGHT_BLACK       = "\e[90m",
	BRIGHT_RED         = "\e[91m",
	BRIGHT_GREEN       = "\e[92m",
	BRIGHT_YELLOW      = "\e[93m",
	BRIGHT_BLUE        = "\e[94m",
	BRIGHT_MAGENTA     = "\e[95m",
	BRIGHT_CYAN        = "\e[96m",
	BRIGHT_WHITE       = "\e[97m",
	BG_BLACK           = "\e[40m",
	BG_RED             = "\e[41m",
	BG_GREEN           = "\e[42m",
	BG_YELLOW          = "\e[43m",
	BG_BLUE            = "\e[44m",
	BG_MAGENTA         = "\e[45m",
	BG_CYAN            = "\e[46m",
	BG_WHITE           = "\e[47m",
	BG_DEFAULT         = "\e[49m",
	BG_BRIGHT_BLACK    = "\e[100m",
	BG_BRIGHT_RED      = "\e[101m",
	BG_BRIGHT_GREEN    = "\e[102m",
	BG_BRIGHT_YELLOW   = "\e[103m",
	BG_BRIGHT_BLUE     = "\e[104m",
	BG_BRIGHT_MAGENTA  = "\e[105m",
	BG_BRIGHT_CYAN     = "\e[106m",
	BG_BRIGHT_WHITE    = "\e[107m",
}

struct AnsiColor (Printable)
{
	char r, g, b;
	bool bg;
}

fn usz? AnsiColor.to_format(&self, Formatter* fmt) @dynamic
{
	return fmt.printf("\e[%s8;2;%s;%s;%sm", self.bg ? 4 : 3, self.r, self.g, self.b);
}

<*
 24-bit color code

 @return `A struct that, when formatted with '%s', sets the foreground or background colour to the specified rgb value`
*>
fn AnsiColor get_color_rgb(char r, char g, char b, bool bg = false)
{
	return {r, g, b, bg};
}

<*
 24-bit color code

 @return `A struct that, when formatted with '%s', sets the foreground or background colour of printed text to the specified rgb value`
*>
fn AnsiColor get_color(uint rgb, bool bg = false)
{
	return {(char)(rgb >> 16), (char)((rgb & 0x00FF00) >> 8), (char)rgb, bg};
}

<*
 8-bit color code

 @return `the formatting char for the given background color`
*>
macro String color_8bit(char $index, bool $bg = false) @const
{
	int $mode = $bg ? 4 : 3;
	return @sprintf("\e[%s8;5;%sm", $mode, $index);
}

<*
 24-bit color code

 @return `the string for the given foreground color`
*>
macro String color_rgb(char $r, char $g, char $b, bool $bg = false) @const
{
	int $mode = $bg ? 4 : 3;
	return @sprintf("\e[%s8;2;%s;%s;%sm", $mode, $r, $g, $b);
}

<*
 24-bit color code rgb

 @require $rgb <= 0xFF_FF_FF : `Expected a 24 bit RGB value`
 @return `the string char for the given foreground color`
*>
macro String color(uint $rgb, bool $bg = false) @const
{
	int $mode = $bg ? 4 : 3;
	return @sprintf("\e[%s8;2;%s;%s;%sm", $mode, $rgb >> 16, ($rgb & 0xFF00) >> 8, $rgb & 0xFF);
}

<*
 24-bit color code rgb

 @require rgb <= 0xFF_FF_FF : `Expected a 24 bit RGB value`
 @return `the string char for the given foreground color`
*>
fn String make_color(Allocator mem, uint rgb, bool bg = false) @deprecated("use get_color instead")
{
	return make_color_rgb(mem, (char)(rgb >> 16), (char)((rgb & 0xFF00) >> 8), (char)rgb, bg);
}

<*
 24-bit color code rgb

 @require rgb <= 0xFF_FF_FF : `Expected a 24 bit RGB value`
 @return `the string char for the given foreground color`
*>
fn String make_tcolor(uint rgb, bool bg = false) @deprecated("use get_color instead")
{
	return make_color_rgb(tmem, (char)(rgb >> 16), (char)((rgb & 0xFF00) >> 8), (char)rgb, bg);
}

<*
 24-bit color code rgb

 @return `the string char for the given foreground color`
*>
fn String make_color_rgb(Allocator mem, char r, char g, char b, bool bg = false) @deprecated("use get_color_rgb instead")
{
	return string::format(mem, "\e[%s8;2;%s;%s;%sm", bg ? 4 : 3, r, g, b);
}

<*
 24-bit color code rgb

 @return `the string char for the given foreground color`
*>
fn String make_tcolor_rgb(char r, char g, char b, bool bg = false) @deprecated("use get_color_rgb instead")
{
	return string::format(tmem, "\e[%s8;2;%s;%s;%sm", bg ? 4 : 3, r, g, b);
}
