<*
 This module contains utils for handling ASCII characters. They only operate on
 characters corresponding to 0-127.
*>
module std::core::ascii;

macro bool @is_lower(c)  => ASCII_LOOKUP[c].lower;      // Is a-z
macro bool @is_upper(c)  => ASCII_LOOKUP[c].upper;      // Is A-Z
macro bool @is_digit(c)  => ASCII_LOOKUP[c].digit;      // Is 0-9
macro bool @is_bdigit(c) => ASCII_LOOKUP[c].bin_digit;  // Is 0-1
macro bool @is_odigit(c) => ASCII_LOOKUP[c].oct_digit;  // Is 0-7
macro bool @is_xdigit(c) => ASCII_LOOKUP[c].hex_digit;  // Is 0-9 or a-f or A-F
macro bool @is_alpha(c)  => ASCII_LOOKUP[c].alpha;      // Is a-z or A-Z
macro bool @is_print(c)  => ASCII_LOOKUP[c].printable;  // Is a printable character (space or higher and < 127
macro bool @is_graph(c)  => ASCII_LOOKUP[c].graph;      // Does it show any graphics (printable but not space)
macro bool @is_space(c)  => ASCII_LOOKUP[c].space;      // Is it a space character: space, tab, linefeed etc
macro bool @is_alnum(c)  => ASCII_LOOKUP[c].alphanum;   // Is it alpha or digit
macro bool @is_punct(c)  => ASCII_LOOKUP[c].punct;      // Is it "graph" but not digit or letter
macro bool @is_blank(c)  => ASCII_LOOKUP[c].blank;      // Is it a blank space: space or tab
macro bool @is_cntrl(c)  => ASCII_LOOKUP[c].control;    // Is it a control character: before space or 127
macro char @to_lower(c)  => c + TO_LOWER[c];            // Convert A-Z to a-z if found
macro char @to_upper(c)  => c - TO_UPPER[c];            // Convert a-z to A-Z if found

fn bool is_lower(char c)  => @is_lower(c);  // Is a-z
fn bool is_upper(char c)  => @is_upper(c);  // Is A-Z
fn bool is_digit(char c)  => @is_digit(c);  // Is 0-9
fn bool is_bdigit(char c) => @is_bdigit(c); // Is 0-1
fn bool is_odigit(char c) => @is_odigit(c); // Is 0-7
fn bool is_xdigit(char c) => @is_xdigit(c); // Is 0-9 or a-f or A-F
fn bool is_alpha(char c)  => @is_alpha(c);  // Is a-z or A-Z
fn bool is_print(char c)  => @is_print(c);  // Is a printable character (space or higher and < 127
fn bool is_graph(char c)  => @is_graph(c);  // Does it show any graphics (printable but not space)
fn bool is_space(char c)  => @is_space(c);  // Is it a space character: space, tab, linefeed etc
fn bool is_alnum(char c)  => @is_alnum(c);  // Is it alpha or digit
fn bool is_punct(char c)  => @is_punct(c);  // Is it "graph" but not digit or letter
fn bool is_blank(char c)  => @is_blank(c);  // Is it a blank space: space or tab
fn bool is_cntrl(char c)  => @is_cntrl(c);  // Is it a control character: before space or 127
fn char to_lower(char c)  => @to_lower(c);  // Convert A-Z to a-z if found
fn char to_upper(char c)  => @to_upper(c);  // Convert a-z to A-Z if found

// The following methods are macro methods for the same functions
macro bool char.is_lower(char c)  => @is_lower(c);
macro bool char.is_upper(char c)  => @is_upper(c);
macro bool char.is_digit(char c)  => @is_digit(c);
macro bool char.is_bdigit(char c) => @is_bdigit(c);
macro bool char.is_odigit(char c) => @is_odigit(c);
macro bool char.is_xdigit(char c) => @is_xdigit(c);
macro bool char.is_alpha(char c)  => @is_alpha(c);
macro bool char.is_print(char c)  => @is_print(c);
macro bool char.is_graph(char c)  => @is_graph(c);
macro bool char.is_space(char c)  => @is_space(c);
macro bool char.is_alnum(char c)  => @is_alnum(c);
macro bool char.is_punct(char c)  => @is_punct(c);
macro bool char.is_blank(char c)  => @is_blank(c);
macro bool char.is_cntrl(char c)  => @is_cntrl(c);
macro char char.to_lower(char c)  => @to_lower(c);
macro char char.to_upper(char c)  => @to_upper(c);

<*
 Convert a-f/A-F/0-9 to the appropriate hex value.

 @require c.is_xdigit()
 @ensure return >= 0 && return <= 15
*>
macro char char.from_hex(char c) => HEX_VALUE[c];

<*
 Bitstruct containing the different properties of a character
*>
bitstruct CharType : ushort @private
{
	bool lower;
	bool upper;
	bool digit;
	bool bin_digit;
	bool hex_digit;
	bool oct_digit;
	bool alpha;
	bool alphanum;
	bool space;
	bool printable;
	bool blank;
	bool punct;
	bool control;
	bool graph;
}

const CharType[256] ASCII_LOOKUP @private = {
	[0..31]    = { .control },
	[9..13]    = { .control, .space },
	['\t']     = { .control, .space, .blank },
	[' ']      = { .space, .printable, .blank },
	[33..126]  = { .printable, .graph, .punct },
	['0'..'9'] = { .printable, .graph, .alphanum, .hex_digit, .digit },
	['2'..'7'] = { .printable, .graph, .alphanum, .hex_digit, .digit, .oct_digit },
	['0'..'1'] = { .printable, .graph, .alphanum, .hex_digit, .digit, .oct_digit, .bin_digit },
	['A'..'Z'] = { .printable, .graph, .alphanum, .alpha, .upper },
	['A'..'F'] = { .printable, .graph, .alphanum, .alpha, .upper, .hex_digit },
	['a'..'z'] = { .printable, .graph, .alphanum, .alpha, .lower },
	['a'..'f'] = { .printable, .graph, .alphanum, .alpha, .lower, .hex_digit },
	[127]      = { .control },
};

const char[256] HEX_VALUE = {
	['0'] =  0, ['1'] =  1, ['2'] =  2, ['3'] =  3, ['4'] =  4,
	['5'] =  5, ['6'] =  6, ['7'] =  7, ['8'] =  8, ['9'] =  9,
	['A'] = 10, ['B'] = 11, ['C'] = 12, ['D'] = 13, ['E'] = 14,
	['F'] = 15, ['a'] = 10, ['b'] = 11, ['c'] = 12, ['d'] = 13,
	['e'] = 14, ['f'] = 15
};

const char[256] TO_UPPER @private = { ['a'..'z'] = 'a' - 'A' };
const char[256] TO_LOWER @private  = { ['A'..'Z'] = 'a' - 'A' };

typedef AsciiCharset = uint128;

macro AsciiCharset @create_set(String $string) @const
{
	AsciiCharset $set;
	$foreach $c : $string:
		$set |= 1ULL << $c;
	$endforeach
	return $set;
}

fn AsciiCharset create_set(String string)
{
	AsciiCharset set;
	foreach (c : string) set |= (AsciiCharset)1ULL << c;
	return set;
}

macro bool AsciiCharset.@contains($set, char $c) @const => !!($c < 128) & !!($set & (AsciiCharset)(1ULL << $c));

macro AsciiCharset @combine_sets(AsciiCharset $first, AsciiCharset... $sets) @const
{
	var $res = $first;
	$foreach $c : $sets:
		$res |= $c;
	$endforeach
	return $res;
}
fn AsciiCharset combine_sets(AsciiCharset first, AsciiCharset... sets)
{
	foreach (c : sets) first |= c;
	return first;
}

macro bool AsciiCharset.contains(set, char c) => !!(c < 128) & !!(set & (AsciiCharset)(1ULL << c));

const AsciiCharset WHITESPACE_SET = @create_set("\t\n\v\f\r ");
const AsciiCharset NUMBER_SET = @create_set("0123456789");
const AsciiCharset ALPHA_UPPER_SET = @create_set("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
const AsciiCharset ALPHA_LOWER_SET = @create_set("abcdefghijklmnopqrstuvwxyz");
const AsciiCharset ALPHA_SET = @combine_sets(ALPHA_UPPER_SET, ALPHA_LOWER_SET);
const AsciiCharset ALPHANUMERIC_SET = @combine_sets(ALPHA_SET, NUMBER_SET);
