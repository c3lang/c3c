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

fn usz! str_index_of(char[] s, char[] needle)
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

fn ZString copy_zstring(char[] s)
{
    usz len = s.len;
    char* str = malloc(len + 1);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
}

fn ZString tcopy_zstring(char[] s)
{
    usz len = s.len;
    char* str = tmalloc(len + 1);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
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
	return data[0..codepoints - 1];
}

fn char[] utf32to8(Char32[] utf32, Allocator* allocator = mem::current_allocator)
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = allocator.alloc(len + 1)!!;
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return data[0..len - 1];
}

fn Char16[]! utf8to16(char[] utf8, Allocator* allocator = mem::current_allocator)
{
	usz len16 = conv::utf16len_for_utf8(utf8);
	Char16* data = allocator.alloc((len16 + 1) * Char16.sizeof)?;
	conv::utf8to16_unsafe(utf8, data)?;
	data[len16] = 0;
	return data[0..len16 - 1];
}


fn char[]! utf16to8(Char16[] utf16, Allocator* allocator = mem::current_allocator())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = allocator.alloc(len + 1)?;
	conv::utf16to8_unsafe(utf16, data)?;
	return data[0 .. len - 1];
}

fn char[] copy(char[] s)
{
    usz len = s.len;
    ZString str_copy = copy_zstring(s) @inline;
    return str_copy[..len];
}

fn char[] tcopy(char[] s)
{
    usz len = s.len;
    ZString str_copy = tcopy_zstring(s) @inline;
    return str_copy[..len];
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

