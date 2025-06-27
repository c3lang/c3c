// Copyright (c) 2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

<*
 This module provides functionality for escaping and unescaping strings
 with standard C-style escape sequences, similar to what's used in JSON
 and other string literals.
*>
module std::core::string_escape;
import std::io;

faultdef INVALID_ESCAPE_SEQUENCE, UNTERMINATED_STRING, INVALID_HEX_ESCAPE, INVALID_UNICODE_ESCAPE;

<*
 Escape a string by adding quotes and converting special characters to escape sequences.

 @param allocator : "The allocator to use for the result"
 @param s : "The string to escape"
 @return "The escaped string with surrounding quotes"
*>
fn String escape(Allocator allocator, String s)
{
    // Conservative allocation: most strings need minimal escaping
    usz initial_capacity = s.len + s.len / 5 + 2; // ~1.2x + quotes
    DString result = dstring::new_with_capacity(allocator, initial_capacity);

    result.append_char('"');

    foreach (char c : s)
    {
        switch (c)
        {
            case '"':  result.append(`\"`);
            case '\\': result.append(`\\`);
            case '\b': result.append(`\b`);
            case '\f': result.append(`\f`);
            case '\n': result.append(`\n`);
            case '\r': result.append(`\r`);
            case '\t': result.append(`\t`);
            case '\v': result.append(`\v`);
            case '\0': result.append(`\0`);
            default:
                if (c >= 32 && c <= 126)
                {
                    // Printable ASCII
                    result.append_char(c);
                }
                else
                {
                    // Non-printable, use hex escape
                    result.appendf("\\x%02x", (uint)c);
                }
        }
    }

    result.append_char('"');
    return result.str_view().copy(allocator);
}

<*
 Escape a string using the temp allocator.

 @param s : "The string to escape"
 @return "The escaped string with surrounding quotes"
*>
fn String tescape(String s) => escape(tmem, s);

<*
 Calculate the length needed for an escaped string (including quotes).

 @param s : "The string to check"
 @return "The length needed for the escaped version"
*>
fn usz escape_len(String s)
{
    usz len = 2; // For quotes
    foreach (char c : s)
    {
        switch (c)
        {
            case '"':
            case '\\':
            case '\b':
            case '\f':
            case '\n':
            case '\r':
            case '\t':
            case '\v':
            case '\0':
                len += 2; // \X
            default:
                if (c >= 32 && c <= 126)
                {
                    len += 1;
                }
                else
                {
                    len += 4; // \xHH
                }
        }
    }
    return len;
}

<*
 Unescape a quoted string by parsing escape sequences.

 @param allocator : "The allocator to use for the result"
 @param s : "The quoted string to unescape (must start and end with quotes)"
 @return "The unescaped string without quotes"
 @return? UNTERMINATED_STRING, INVALID_ESCAPE_SEQUENCE, INVALID_HEX_ESCAPE, INVALID_UNICODE_ESCAPE
*>
fn String? unescape(Allocator allocator, String s)
{
    if (s.len < 2 || s[0] != '"' || s[^1] != '"')
    {
        return UNTERMINATED_STRING?;
    }

    // Handle empty string case
    if (s.len == 2)
    {
        return "";
    }

    // Remove surrounding quotes (end index is inclusive)
    String content = s[1..^2];
    DString result = dstring::new_with_capacity(allocator, content.len);

    for (usz i = 0; i < content.len; i++)
    {
        char c = content[i];
        if (c != '\\')
        {
            result.append_char(c);
            continue;
        }

        // Handle escape sequence
        if (i + 1 >= content.len) return INVALID_ESCAPE_SEQUENCE?;

        char escape_char = content[++i];
        switch (escape_char)
        {
            case '"':  result.append_char('"');
            case '\\': result.append_char('\\');
            case '/':  result.append_char('/');
            case 'b':  result.append_char('\b');
            case 'f':  result.append_char('\f');
            case 'n':  result.append_char('\n');
            case 'r':  result.append_char('\r');
            case 't':  result.append_char('\t');
            case 'v':  result.append_char('\v');
            case '0':  result.append_char('\0');
            case 'x':
                // Hex escape \xHH
                if (i + 2 >= content.len) return INVALID_HEX_ESCAPE?;
                char h1 = content[++i];
                char h2 = content[++i];
                if (!h1.is_xdigit() || !h2.is_xdigit()) return INVALID_HEX_ESCAPE?;
                uint val = h1 > '9' ? (h1 | 32) - 'a' + 10 : h1 - '0';
                val = val << 4;
                val += h2 > '9' ? (h2 | 32) - 'a' + 10 : h2 - '0';
                result.append_char((char)val);
            case 'u':
                // Unicode escape \uHHHH
                if (i + 4 >= content.len) return INVALID_UNICODE_ESCAPE?;
                uint val;
                for (int j = 0; j < 4; j++)
                {
                    char hex_char = content[++i];
                    if (!hex_char.is_xdigit()) return INVALID_UNICODE_ESCAPE?;
                    val = val << 4 + (hex_char > '9' ? (hex_char | 32) - 'a' + 10 : hex_char - '0');
                }
                result.append_char32(val);
            case 'U':
                // Unicode escape \UHHHHHHHH
                if (i + 8 >= content.len) return INVALID_UNICODE_ESCAPE?;
                uint val;
                for (int j = 0; j < 8; j++)
                {
                    char hex_char = content[++i];
                    if (!hex_char.is_xdigit()) return INVALID_UNICODE_ESCAPE?;
                    val = val << 4 + (hex_char > '9' ? (hex_char | 32) - 'a' + 10 : hex_char - '0');
                }
                result.append_char32(val);
            default:
                return INVALID_ESCAPE_SEQUENCE?;
        }
    }

    return result.str_view().copy(allocator);
}

<*
 Unescape a quoted string using the temp allocator.

 @param s : "The quoted string to unescape"
 @return "The unescaped string without quotes"
 @return? UNTERMINATED_STRING, INVALID_ESCAPE_SEQUENCE, INVALID_HEX_ESCAPE, INVALID_UNICODE_ESCAPE
*>
fn String? tunescape(String s) => unescape(tmem, s);

<*
 Check if a character needs to be escaped in a string literal.

 @param c : "The character to check"
 @return "True if the character needs escaping"
*>
fn bool needs_escape(char c)
{
    switch (c)
    {
        case '"':
        case '\\':
        case '\b':
        case '\f':
        case '\n':
        case '\r':
        case '\t':
        case '\v':
        case '\0':
            return true;
        default:
            return c < 32 || c > 126;
    }
}

<*
 Escape a string without adding surrounding quotes.

 @param allocator : "The allocator to use for the result"
 @param s : "The string to escape"
 @return "The escaped string without quotes"
*>
fn String escape_content(Allocator allocator, String s)
{
    // Conservative allocation: most strings need minimal escaping
    usz initial_capacity = s.len + s.len / 5; // ~1.2x
    DString result = dstring::new_with_capacity(allocator, initial_capacity);

    foreach (char c : s)
    {
        switch (c)
        {
            case '"':  result.append(`\"`);
            case '\\': result.append(`\\`);
            case '\b': result.append(`\b`);
            case '\f': result.append(`\f`);
            case '\n': result.append(`\n`);
            case '\r': result.append(`\r`);
            case '\t': result.append(`\t`);
            case '\v': result.append(`\v`);
            case '\0': result.append(`\0`);
            default:
                if (c >= 32 && c <= 126)
                {
                    result.append_char(c);
                }
                else
                {
                    result.appendf("\\x%02x", (uint)c);
                }
        }
    }

    return result.str_view().copy(allocator);
}

<*
 Escape a string content using the temp allocator.

 @param s : "The string to escape"
 @return "The escaped string without quotes"
*>
fn String tescape_content(String s) => escape_content(tmem, s);
