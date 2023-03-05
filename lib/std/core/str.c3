module std::core::str;

typedef ZString = distinct char*;
typedef String = char[];
typedef Char32 = uint;
typedef Char16 = ushort;

const uint SURROGATE_OFFSET @private = 0x10000;
const uint SURROGATE_GENERIC_MASK @private = 0xF800;
const uint SURROGATE_MASK @private = 0xFC00;
const uint SURROGATE_CODEPOINT_MASK @private = 0x03FF;
const uint SURROGATE_BITS @private = 10;
const uint SURROGATE_LOW_VALUE @private = 0xDC00;
const uint SURROGATE_HIGH_VALUE @private = 0xD800;

fault NumberConversion
{
	EMPTY_STRING,
	NEGATIVE_VALUE,
	MALFORMED_INTEGER,
	INTEGER_OVERFLOW,
	MALFORMED_FLOAT,
	FLOAT_OUT_OF_RANGE,
}

fn VarString join(String[] s, String joiner)
{
	if (!s.len) return (VarString)null;
	usz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	VarString res = string::new_with_capacity(total_size);
	res.append(s[0]);
	foreach (String* &str : s[1..])
	{
		res.append(joiner);
		res.append(*str);
	}
	return res;
}

macro bool char_in_set(char c, String set)
{
	foreach (ch : set)
	{
		if (ch == c) return true;
	}
	return false;
}

macro char_is_space_tab(char c) @private
{
	return c == ' ' || c == '\t';
}

macro to_integer($Type, String string) @private
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

fn float! to_float(String string) => floatparse(string, float);
fn double! to_double(String string) => floatparse(string, double);
fn int128! to_int128(String string) => to_integer(int128, string);
fn long! to_long(String string) => to_integer(long, string);
fn int! to_int(String string) => to_integer(int, string);
fn short! to_short(String string) => to_integer(short, string);
fn ichar! to_ichar(String string) => to_integer(ichar, string);

fn uint128! to_uint128(String str) => to_integer(uint128, str);
fn ulong! to_ulong(String str) => to_integer(ulong, str);
fn uint! to_uint(String str) => to_integer(uint, str);
fn ushort! to_ushort(String str) => to_integer(ushort, str);
fn char! to_uchar(String str) => to_integer(char, str);

fn String trim(String string, String to_trim = "\t\n\r ")
{
	usz start = 0;
	usz len = string.len;
	while (start < len && char_in_set(string[start], to_trim)) start++;
	if (start == len) return string[:0];
	usz end = len - 1;
	while (end > start && char_in_set(string[end], to_trim)) end--;
	return string[start..end];
}

fn bool starts_with(String s, String needle)
{
	if (needle.len > s.len) return false;
	foreach (i, c : needle)
	{
		if (c != s[i]) return false;
	}
	return true;
}

fn String[] tsplit(String s, String needle) => split(s, needle, mem::temp_allocator()) @inline;

fn String[] split(String s, String needle, Allocator* allocator = mem::current_allocator())
{
	usz capacity = 16;
	usz i = 0;
	String* holder = malloc(String, capacity, .using = allocator);
	while (s.len)
	{
		usz! index = index_of(s, needle);
		String res @noinit;
		if (try index)
		{
			res = s[:index];
			s = s[index + needle.len..];
		}
		else
		{
			res = s;
			s = s[:0];
		}
		if (i == capacity)
		{
			capacity *= 2;
			holder = realloc(holder, String.sizeof * capacity, .using = allocator);
		}
		holder[i++] = res;
	}
	return holder[:i];
}

fn usz! rindex_of(String s, String needle)
{
	usz match = 0;
	usz needed = needle.len;
	if (!needed) return SearchResult.MISSING!;
	usz index_start = 0;
	char search = needle[0];
	foreach_r (usz i, char c : s)
	{
		if (c == search)
		{
			if (!match) index_start = i;
			match++;
			if (match == needed) return index_start;
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

fn usz! index_of(String s, String needle)
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
			if (match == needed) return index_start;
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

fn ZString String.zstrcopy(String s, Allocator* allocator = mem::current_allocator()) => copy_zstring(s, allocator) @inline;
fn ZString String.zstrtcopy(String s) => copy_zstring(s, mem::temp_allocator()) @inline;
fn String String.copyz(String s, Allocator* allocator = mem::current_allocator()) => copyz(s, allocator) @inline;
fn String String.tcopyz(String s) => copyz(s, mem::temp_allocator()) @inline;

fn ZString copy_zstring(String s, Allocator* allocator = mem::current_allocator())
{
    usz len = s.len;
    char* str = malloc(len + 1, .using = allocator);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
}


fn String copyz(String s, Allocator* allocator = mem::current_allocator())
{
    usz len = s.len;
    char* str = malloc(len + 1, .using = allocator);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return str[:len];
}

fn ZString tcopy_zstring(String s)
{
	return copy_zstring(s, mem::temp_allocator());
}

fn bool compare(String a, String b)
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

fn usz utf8_codepoints(String utf8)
{
	usz len = 0;
	foreach (char c : utf8)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

fn Char32[]! utf8to32(String utf8, Allocator* allocator = mem::current_allocator())
{
	usz codepoints = conv::utf8_codepoints(utf8);
	Char32* data = malloc_checked(Char32, codepoints + 1, .using = allocator)?;
	conv::utf8to32_unsafe(utf8, data)?;
	data[codepoints] = 0;
	return data[:codepoints];
}

fn String utf32to8(Char32[] utf32, Allocator* allocator = mem::current_allocator())
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = malloc_checked(len + 1, .using = allocator)!!;
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return data[:len];
}

fn Char16[]! utf8to16(String utf8, Allocator* allocator = mem::current_allocator())
{
	usz len16 = conv::utf16len_for_utf8(utf8);
	Char16* data = malloc_checked(Char16, len16 + 1, .using = allocator)?;
	conv::utf8to16_unsafe(utf8, data)?;
	data[len16] = 0;
	return data[:len16];
}


fn String! utf16to8(Char16[] utf16, Allocator* allocator = mem::current_allocator())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = malloc_checked(len + 1, .using = allocator)?;
	conv::utf16to8_unsafe(utf16, data)?;
	data[len] = 0;
	return data[:len];
}

	fn String copy(String s, Allocator* allocator = mem::current_allocator())
{
    usz len = s.len;
    ZString str_copy = copy_zstring(s, allocator) @inline;
    return str_copy[:len];
}

fn String tcopy(String s)
{
    usz len = s.len;
    ZString str_copy = tcopy_zstring(s) @inline;
    return str_copy[:len];
}

fn String tconcat(String s1, String s2)
{
    usz full_len = s1.len + s2.len;
    char* str = tmalloc(full_len + 1);
    usz s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[:full_len];
}

fn String concat(String s1, String s2)
{
    usz full_len = s1.len + s2.len;
    char* str = malloc(full_len + 1);
    usz s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[:full_len];
}

fn String ZString.as_str(ZString str)
{
	return ((char*)str)[:str.len()];
}

fn usz ZString.char_len(ZString str)
{
	usz len = 0;
	char* ptr = (char*)str;
	while (char c = ptr++[0])
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

fn usz ZString.len(ZString str)
{
	usz len = 0;
    char* ptr = (char*)str;
    while (char c = ptr++[0]) len++;
    return len;
}

