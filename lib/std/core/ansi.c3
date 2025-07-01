module std::core::ansi;

import std::core::string;

enum AnsiColor : const inline String
{
    RESET             = "\u001B[0m",
    BOLD              = "\u001B[1m",
    DIM               = "\u001B[2m",
    ITALIC            = "\u001B[3m",
    UNDERLINE         = "\u001B[4m",
    BLINK             = "\u001B[5m",
    BLINK_FAST        = "\u001B[6m",
    REVERSE           = "\u001B[7m",
    HIDDEN            = "\u001B[8m",
    STRIKETHROUGH     = "\u001B[9m",
    BLACK             = "\u001B[30m",
    RED               = "\u001B[31m",
    GREEN             = "\u001B[32m",
    YELLOW            = "\u001B[33m",
    BLUE              = "\u001B[34m",
    MAGENTA           = "\u001B[35m",
    CYAN              = "\u001B[36m",
    WHITE             = "\u001B[37m",
    DEFAULT           = "\u001B[39m",
    BRIGHT_BLACK      = "\u001B[90m",
    BRIGHT_RED        = "\u001B[91m",
    BRIGHT_GREEN      = "\u001B[92m",
    BRIGHT_YELLOW     = "\u001B[93m",
    BRIGHT_BLUE       = "\u001B[94m",
    BRIGHT_MAGENTA    = "\u001B[95m",
    BRIGHT_CYAN       = "\u001B[96m",
    BRIGHT_WHITE      = "\u001B[97m",
    BG_BLACK          = "\u001B[40m",
    BG_RED            = "\u001B[41m",
    BG_GREEN          = "\u001B[42m",
    BG_YELLOW         = "\u001B[43m",
    BG_BLUE           = "\u001B[44m",
    BG_MAGENTA        = "\u001B[45m",
    BG_CYAN           = "\u001B[46m",
    BG_WHITE          = "\u001B[47m",
    BG_DEFAULT        = "\u001B[49m",
    BG_BRIGHT_BLACK   = "\u001B[100m",
    BG_BRIGHT_RED     = "\u001B[101m",
    BG_BRIGHT_GREEN   = "\u001B[102m",
    BG_BRIGHT_YELLOW  = "\u001B[103m",
    BG_BRIGHT_BLUE    = "\u001B[104m",
    BG_BRIGHT_MAGENTA = "\u001B[105m",
    BG_BRIGHT_CYAN    = "\u001B[106m",
    BG_BRIGHT_WHITE   = "\u001B[107m",
}

<*
 8-bit color code

 @require index >= 0 && index <= 255 : `value must be within range 0-255`
 @return `the formatting char for the given background color`
*>
fn String color_8bit_fg(int index)
{
    return string::format(mem, "\u001B[38;5;%dm", index);
}

<*
 8-bit color code

 @require index >= 0 && index <= 255 : `value must be within range 0-255`
 @return `the formatting char for the given background color`
*>
fn String color_8bit_bg(int index)
{
    return string::format(mem, "\u001B[48;5;%dm", index);
}

<*
 24-bit color code

 @require r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255: `value must be within range 0-255`
 @return `the formatting char for the given foreground color`
*>
fn String rgb_fg(int r, int g, int b)
{
    return string::format(mem, "\u001B[38;2;%d;%d;%dm", r, g, b);
}

<*
 24-bit color code

 @require r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255: `value must be within range 0-255`
 @return `the formatting char for the given background color`
*>
fn String rgb_bg(int r, int g, int b)
{
    return string::format(mem, "\u001B[48;2;%d;%d;%dm", r, g, b);
}
