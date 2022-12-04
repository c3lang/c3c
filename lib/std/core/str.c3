module std::core::str;
define ZString = distinct char*;
define Char32 = uint;
define Char16 = ushort;

private const uint SURROGATE_OFFSET = 0x10000;
private const uint SURROGATE_GENERIC_MASK = 0xF800;
private const uint SURROGATE_MASK = 0xFC00;
private const uint SURROGATE_CODEPOINT_MASK = 0x03FF;
private const uint SURROGATE_BITS = 10;
private const uint SURROGATE_LOW_VALUE = 0xDC00;
private const uint SURROGATE_HIGH_VALUE = 0xD800;

fault NumberConversion
{
	EMPTY_STRING,
	NEGATIVE_VALUE,
	MALFORMED_INTEGER,
	INTEGER_OVERFLOW,
}
fn String join(char[][] s, char[] joiner)
{
	if (!s.len) return (String)null;
	usz total_size = joiner.len * s.len;
	foreach (char[]* &str : s)
	{
		total_size += str.len;
	}
	String res = string::new_with_capacity(total_size);
	res.append(s[0]);
	foreach (char[]* &str : s[1..])
	{
		res.append(joiner);
		res.append(*str);
	}
	return res;
}

macro bool char_in_set(char c, char[] set)
{
	foreach (ch : set)
	{
		if (ch == c) return true;
	}
	return false;
}

private macro char_is_space_tab(char c)
{
	return c == ' ' || c == '\t';
}

private macro to_signed_integer($Type, char[] string)
{
	usz len = string.len;
	usz index = 0;
	char* ptr = string.ptr;
	while (index < len && char_is_space_tab(ptr[index])) index++;
	if (len == index) return NumberConversion.EMPTY_STRING!;
	bool is_negative;
	switch (string[index])
	{
		case '-':
			if ($Type.min == 0) return NumberConversion.NEGATIVE_VALUE!;
			is_negative = true;
			index++;
		case '+':
			index++;
		default:
			break;
	}
	if (len == index) return NumberConversion.MALFORMED_INTEGER!;
	$Type base = 10;
	if (string[index] == '0')
	{
		index++;
		if (index == len) return ($Type)0;
		switch (string[index])
		{
			case 'x':
			case 'X':
				base = 16;
				index++;
			case 'b':
			case 'B':
				base = 2;
				index++;
			case 'o':
			case 'O':
				base = 8;
				index++;
			default:
				break;
		}
		if (len == index) return NumberConversion.MALFORMED_INTEGER!;
	}
	$Type value = 0;
	while (index != len)
	{
		char c = {|
			char ch = string[index++];
			if (base != 16 || ch < 'A') return (char)(ch - '0');
			if (ch <= 'F') return (char)(ch - 'A');
			if (ch < 'a') return NumberConversion.MALFORMED_INTEGER!;
			if (ch > 'f') return NumberConversion.MALFORMED_INTEGER!;
			return (char)(ch - 'a');
		|}?;
		if (c >= base) return NumberConversion.MALFORMED_INTEGER!;
		value = {|
			if (is_negative)
			{
				$Type new_value = value * base - c;
				if (new_value > value) return NumberConversion.INTEGER_OVERFLOW!;
				return new_value;
			}
			$Type new_value = value * base + c;
			if (new_value < value) return NumberConversion.INTEGER_OVERFLOW!;
			return new_value;
		|}?;
	}
	return value;
}

fn int128! to_int128(char[] string) = to_signed_integer(int128, string);
fn long! to_long(char[] string) = to_signed_integer(long, string);
fn int! to_int(char[] string) = to_signed_integer(int, string);
fn short! to_short(char[] string) = to_signed_integer(short, string);
fn ichar! to_ichar(char[] string) = to_signed_integer(ichar, string);

fn char[] trim(char[] string, char[] to_trim = "\t\n\r ")
{
	usz start = 0;
	usz len = string.len;
	while (start < len && char_in_set(string[start], to_trim)) start++;
	if (start == len) return string[:0];
	usz end = len - 1;
	while (end > start && char_in_set(string[end], to_trim)) end--;
	return string[start..end];
}

fn bool starts_with(char[] s, char[] needle)
{
	if (needle.len > s.len) return false;
	foreach (i, c : needle)
	{
		if (c != s[i]) return false;
	}
	return true;
}

fn char[][] tsplit(char[] s, char[] needle) = split(s, needle, mem::temp_allocator()) @inline;

fn char[][] split(char[] s, char[] needle, Allocator* allocator = mem::current_allocator())
{
	usz capacity = 16;
	usz i = 0;
	char[]* holder = allocator.alloc(char[].sizeof * capacity)!!;
	while (s.len)
	{
		usz! index = index_of(s, needle);
		char[] res = void;
		if (try index)
		{
			res = s[:index];
			s = s[index + 1..];
		}
		else
		{
			res = s;
			s = s[:0];
		}
		if (i == capacity)
		{
			capacity *= 2;
			holder = allocator.realloc(holder, capacity)!!;
		}
		holder[i++] = res;
	}
	return holder[:i];
}

fn usz! index_of(char[] s, char[] needle)
{
	usz match = 0;
	usz needed = needle.len;
	if (!needed) return SearchResult.MISSING!;
	usz index_start = 0;
	char search = needle[0];
	foreach (usz i, char c : s)
	{
		if (c == search)
		{
			if (!match) index_start = i;
			match++;
			if (match == needed) return i;
			search = needle[match];
			continue;
		}
		if (match)
		{
			match = 0;
			search = needle[0];
		}
	}
	return SearchResult.MISSING!;
}

fn ZString copy_zstring(char[] s, Allocator* allocator = mem::current_allocator())
{
    usz len = s.len;
    char* str = allocator.alloc(len + 1)!!;
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
}

fn ZString tcopy_zstring(char[] s)
{
	return copy_zstring(s, mem::temp_allocator());
}

fn bool compare(char[] a, char[] b)
{
	if (a.len != b.len) return false;
	foreach (i, c : a)
	{
		if (c != b[i]) return false;
	}
	return true;
}
fault UnicodeResult
{
	INVALID_UTF8,
	INVALID_UTF16,
	CONVERSION_FAILED,
}

fn usz utf8_codepoints(char[] utf8)
{
	usz len = 0;
	foreach (char c : utf8)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

fn Char32[]! utf8to32(char[] utf8, Allocator* allocator = mem::current_allocator)
{
	usz codepoints = conv::utf8_codepoints(utf8);
	Char32* data = allocator.alloc(Char32.sizeof * (codepoints + 1))?;
	conv::utf8to32_unsafe(utf8, data)?;
	data[codepoints] = 0;
	return data[:codepoints];
}

fn char[] utf32to8(Char32[] utf32, Allocator* allocator = mem::current_allocator)
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = allocator.alloc(len + 1)!!;
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return data[:len];
}

fn Char16[]! utf8to16(char[] utf8, Allocator* allocator = mem::current_allocator)
{
	usz len16 = conv::utf16len_for_utf8(utf8);
	Char16* data = allocator.alloc((len16 + 1) * Char16.sizeof)?;
	conv::utf8to16_unsafe(utf8, data)?;
	data[len16] = 0;
	return data[:len16];
}


fn char[]! utf16to8(Char16[] utf16, Allocator* allocator = mem::current_allocator())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = allocator.alloc(len + 1)?;
	conv::utf16to8_unsafe(utf16, data)?;
	return data[:len];
}

fn char[] copy(char[] s, Allocator* allocator = mem::current_allocator())
{
    usz len = s.len;
    ZString str_copy = copy_zstring(s, allocator) @inline;
    return str_copy[:len];
}

fn char[] tcopy(char[] s)
{
    usz len = s.len;
    ZString str_copy = tcopy_zstring(s) @inline;
    return str_copy[:len];
}

fn char[] tconcat(char[] s1, char[] s2)
{
    usz full_len = s1.len + s2.len;
    char* str = tmalloc(full_len + 1);
    usz s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}

fn char[] concat(char[] s1, char[] s2)
{
    usz full_len = s1.len + s2.len;
    char* str = malloc(full_len + 1);
    usz s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}

fn usz ZString.len(ZString *str)
{
	usz len = 0;
	char* ptr = (char*)*str;
	while (char c = ptr++[0])
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

