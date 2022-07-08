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

fault UnicodeResult
{
	INVALID_UTF8,
	INVALID_UTF16,
}

/**
 * @param c `The utf32 codepoint to convert`
 * @param [out] output `the resulting buffer`
 *
 * @return `the number of characters written 1-4`
 **/
fn char char32_to_utf8(Char32 c, char* output)
{
	if (c < 0x7f)
	{
		output[0] = (char)c;
		return 1;
    }
    if (c < 0x7ff)
    {
		output[0] = (char)(0xC0 | c >> 6);
        output[1] = (char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c < 0xffff)
    {
        output[0] = (char)(0xE0 | c >> 12);
        output[1] = (char)(0x80 | (c >> 6 & 0x3F));
        output[2] = (char)(0x80 | (c & 0x3F));
        return 3;
    }
    output[0] = (char)(0xF0 | c >> 18);
    output[1] = (char)(0x80 | (c >> 12 & 0x3F));
    output[2] = (char)(0x80 | (c >> 6 & 0x3F));
    output[3] = (char)(0x80 | (c & 0x3F));
    return 4;
}

fn Char32! utf8CharTo32(char* ptr, int* size)
{
	int max_size = *size;
	if (max_size < 1) return UnicodeResult.INVALID_UTF8!;
	char c = (ptr++)[0];

    if ((c & 0x80) == 0)
    {
        *size = 1;
        return c;
    }
    if ((c & 0xE0) == 0xC0)
    {
		if (max_size < 2) return UnicodeResult.INVALID_UTF8!;
        *size = 2;
        Char32 uc = (c & 0x1F) << 6;
		c = *ptr;
		if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
		return uc + c & 0x3F;
    }
    if ((c & 0xF0) == 0xE0)
    {
		if (max_size < 3) return UnicodeResult.INVALID_UTF8!;
        *size = 3;
        Char32 uc = (c & 0x0F) << 12;
		c = ptr++[0];
		if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
		uc += (c & 0x3F) << 6;
		c = ptr++[0];
		if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
		return uc + c & 0x3F;
    }
    if (max_size < 4) return UnicodeResult.INVALID_UTF8!;
    *size = 4;
    Char32 uc = (c & 0x07) << 18;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
	uc += (c & 0x3F) << 12;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
	uc += (c & 0x3F) << 6;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8!;
	return uc + c & 0x3F;
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

fn Char32[]! utf8to32(char[] utf8, Allocator allocator = { null, null })
{
	if (!allocator.function) allocator = mem::current_allocator();
	usize len = utf8.len;
	Char32* data = allocator.alloc((len + 1) * Char32.sizeof)?;
	usize len32 = 0;
	for (usize i = 0; i < len;)
	{
		int width = (int)min(len - i, 4);
		Char32 uc = utf8CharTo32(&utf8[i], &width) @inline?;
		i += width;
		data[len32++] = uc;
	}
	return data[0 .. len32 - 1];
}


fn Char16[]! utf8to16(char[] utf8, Allocator allocator = { null, null })
{
	if (!allocator.function) allocator = mem::current_allocator();
	usize len = utf8.len;
	Char16* data = allocator.alloc((len + 1) * Char16.sizeof)?;
	usize len16 = 0;
	for (usize i = 0; i < len;)
	{
		int width = (int)min(len - i, 4);
		Char32 uc = utf8CharTo32(&utf8[i], &width) @inline?;
		i += width;
		if (uc <= 0xFFFF)
		{
			data[len16++] = (Char16)uc;
			continue;
		}
		uc -= SURROGATE_OFFSET;
		Char16 low = (Char16)(SURROGATE_LOW_VALUE | (uc & SURROGATE_CODEPOINT_MASK));
		uc >>= SURROGATE_BITS;
		Char16 high = (Char16)(SURROGATE_HIGH_VALUE | (uc & SURROGATE_CODEPOINT_MASK));
		data[len16++] = high;
		data[len16++] = low;
	}
	return data[0 .. len16 - 1];
}


fn char[]! utf16to8(Char16[] utf16, Allocator allocator = { null, null })
{
	if (!allocator.function) allocator = mem::current_allocator();
	String str = string::new_with_capacity(utf16.len * 2 + 1, allocator);
	usize len = utf16.len;
	for (usize i = 0; i < len; i++)
	{
		Char16 high = utf16[i];
		if (high & SURROGATE_GENERIC_MASK != SURROGATE_HIGH_VALUE)
		{
			str.append_char32(high);
			continue;
		}
		// Low surrogate first is an error
		if (high & SURROGATE_MASK != SURROGATE_HIGH_VALUE) return UnicodeResult.INVALID_UTF16!;
		// Unmatched high surrogate is an error
		if (i == len - 1) return UnicodeResult.INVALID_UTF16!;

		Char16 low = utf16[++i];
		// Unmatched high surrogate, invalid
		if (low & SURROGATE_MASK != SURROGATE_LOW_VALUE) return UnicodeResult.INVALID_UTF16!;

        // The high bits of the codepoint are the value bits of the high surrogate
        // The low bits of the codepoint are the value bits of the low surrogate
        Char32 uc = (high & SURROGATE_CODEPOINT_MASK) << SURROGATE_BITS | (low & SURROGATE_CODEPOINT_MASK) + SURROGATE_OFFSET;
		str.append_char32(uc);
	}
	usize new_len = str.len();
	ZString zstr = str.copy_zstr();
	str.destroy();
	return zstr[0 .. new_len - 1];
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