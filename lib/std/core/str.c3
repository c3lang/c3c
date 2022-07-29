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
	usize total_size = joiner.len * s.len;
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


fn ZString copy_zstring(char[] s)
{
    usize len = s.len;
    char* str = mem::alloc(len + 1);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
}

fn ZString tcopy_zstring(char[] s)
{
    usize len = s.len;
    char* str = mem::talloc(len + 1)!!;
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



fn usize utf8_codepoints(char[] utf8)
{
	usize len = 0;
	foreach (char c : utf8)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

fn Char32[]! utf8to32(char[] utf8, Allocator* allocator = mem::current_allocator)
{
	usize codepoints = conv::utf8_codepoints(utf8);
	Char32* data = allocator.alloc(Char32.sizeof * (codepoints + 1))?;
	conv::utf8to32_unsafe(utf8, data)?;
	data[codepoints] = 0;
	return data[0..codepoints - 1];
}

fn char[] utf32to8(Char32[] utf32, Allocator* allocator = mem::current_allocator)
{
	usize len = conv::utf8len_for_utf32(utf32);
	char* data = allocator.alloc(len + 1)!!;
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return data[0..len - 1];
}

fn Char16[]! utf8to16(char[] utf8, Allocator* allocator = mem::current_allocator)
{
	usize len16 = conv::utf16len_for_utf8(utf8);
	Char16* data = allocator.alloc((len16 + 1) * Char16.sizeof)?;
	conv::utf8to16_unsafe(utf8, data)?;
	data[len16] = 0;
	return data[0..len16 - 1];
}


fn char[]! utf16to8(Char16[] utf16, Allocator* allocator = mem::current_allocator())
{
	usize len = conv::utf8len_for_utf16(utf16);
	char* data = allocator.alloc(len + 1)?;
	conv::utf16to8_unsafe(utf16, data)?;
	return data[0 .. len - 1];
}

fn char[] copy(char[] s)
{
    usize len = s.len;
    ZString str_copy = copy_zstring(s) @inline;
    return str_copy[..len];
}

fn char[] tcopy(char[] s)
{
    usize len = s.len;
    ZString str_copy = tcopy_zstring(s) @inline;
    return str_copy[..len];
}

fn char[] tconcat(char[] s1, char[] s2)
{
    usize full_len = s1.len + s2.len;
    char* str = mem::talloc(full_len + 1)!!;
    usize s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}

fn char[] concat(char[] s1, char[] s2)
{
    usize full_len = s1.len + s2.len;
    char* str = mem::alloc(full_len + 1);
    usize s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return str[..full_len];
}

fn usize ZString.len(ZString *str)
{
	usize len = 0;
	char* ptr = (char*)*str;
	while (char c = ptr++[0])
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}