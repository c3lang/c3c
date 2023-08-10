module std::core::string;
import std::ascii;

def ZString = distinct inline char*;
def WString = distinct inline Char16*;
def Char32 = uint;
def Char16 = ushort;

fault UnicodeResult
{
	INVALID_UTF8,
	INVALID_UTF16,
	CONVERSION_FAILED,
}

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

macro String printf(String fmt, ..., Allocator* using = mem::heap())
{
	@pool(using)
	{
		DString str;
		str.tinit();
		str.printf(fmt, $vasplat());
		return str.copy_str(using);
	};
}

macro String tprintf(String fmt, ...)
{
	DString str;
	str.tinit();
	str.printf(fmt, $vasplat());
	return str.as_str();
}

macro bool char_in_set(char c, String set)
{
	foreach (ch : set) if (ch == c) return true;
	return false;
}

fn String join(String[] s, String joiner, Allocator* using = mem::heap())
{
	if (!s)
	{
		return (String)(calloc(char, 2, .using = using)[:0]);
	}

	usz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	@pool(using)
	{
		DString res = dstring::tnew_with_capacity(total_size);
		res.append(s[0]);
		foreach (String* &str : s[1..])
		{
			res.append(joiner);
			res.append(*str);
		}
		return res.copy_str(using);
	};
}

/**
 * @param [in] string
 * @param [in] to_trim
 **/
fn String String.trim(string, String to_trim = "\t\n\r ")
{
	usz start = 0;
	usz len = string.len;
	while (start < len && char_in_set(string[start], to_trim)) start++;
	if (start == len) return string[:0];
	usz end = len - 1;
	while (end > start && char_in_set(string[end], to_trim)) end--;
	return string[start..end];
}

/**
 * @param [in] string
 * @param [in] needle
 **/
fn bool String.starts_with(string, String needle)
{
	if (needle.len > string.len) return false;
	if (!needle.len) return true;
	return string[:needle.len] == needle;
}

/**
 * @param [in] string
 * @param [in] needle
 **/
fn bool String.ends_with(string, String needle)
{
	if (needle.len > string.len) return false;
	if (!needle.len) return true;
	return string[^needle.len..] == needle;
}

/**
 * Strip the front of the string if the prefix exists.
 *
 * @param [in] string
 * @param [in] needle
 **/
fn String String.strip(string, String needle)
{
	if (!needle.len || !string.starts_with(needle)) return string;
	return string[needle.len..];
}

/**
 * Strip the end of the string if the suffix exists.
 *
 * @param [in] string
 * @param [in] needle
 **/
fn String String.strip_end(string, String needle)
{
	if (!needle.len || !string.ends_with(needle)) return string;
	// Note that this is the safe way if we want to support zero length.
	return string[:(string.len - needle.len)];
}


/**
 * Split a string into parts, e.g "a|b|c" split with "|" yields { "a", "b", "c" }
 *
 * @param [in] s
 * @param [in] needle
 * @param [&inout] using "The allocator, defaults to the heap allocator"
 * @param max "Max number of elements, 0 means no limit, defaults to 0"
 * @require needle.len > 0 "The needle must be at least 1 character long"
 * @ensure return.len > 0
 **/
fn String[] String.split(s, String needle, usz max = 0, Allocator* using = mem::heap())
{
	usz capacity = 16;
	usz i = 0;
	String* holder = malloc(String, capacity, .using = using);
	bool no_more = false;
	while (!no_more)
	{
		usz! index = i == max - 1 ? SearchResult.MISSING? : s.index_of(needle);
		String res @noinit;
		if (try index)
		{
			res = s[:index];
			s = s[index + needle.len..];
		}
		else
		{
			res = s;
			no_more = true;
		}
		if (i == capacity)
		{
			capacity *= 2;
			holder = realloc(holder, String.sizeof * capacity, .using = using);
		}
		holder[i++] = res;
	}
	return holder[:i];
}

/**
 * This function is identical to String.split, but implicitly uses the
 * temporary allocator.
 *
 * @param [in] s
 * @param [in] needle
 * @param max "Max number of elements, 0 means no limit, defaults to 0"
 **/
fn String[] String.tsplit(s, String needle, usz max = 0)
{
	return s.split(needle, max, mem::temp()) @inline;
}

fn bool String.contains(s, String needle)
{
	return @ok(s.index_of(needle));
}

/**
 * Find the index of the first incidence of a string.
 *
 * @param [in] s
 * @pure
 * @ensure return < s.len
 * @return "the index of the needle"
 * @return! SearchResult.MISSING "if the needle cannot be found"
 **/
fn usz! String.index_of_char(s, char needle)
{
	foreach (i, c : s)
	{
		if (c == needle) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * Find the index of the first incidence of a string.
 *
 * @param [in] s
 * @pure
 * @ensure return < s.len
 * @return "the index of the needle"
 * @return! SearchResult.MISSING "if the needle cannot be found"
 **/
fn usz! String.rindex_of_char(s, char needle)
{
	foreach_r (i, c : s)
	{
		if (c == needle) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * Find the index of the first incidence of a string.
 *
 * @param [in] s
 * @param [in] needle
 * @pure
 * @ensure return < s.len
 * @require needle.len > 0 : "The needle must be len 1 or more"
 * @return "the index of the needle"
 * @return! SearchResult.MISSING "if the needle cannot be found"
 **/
fn usz! String.index_of(s, String needle)
{
	usz needed = needle.len;
	if (needed > 0 && s.len >= needed)
	{
		char first = needle[0];
		foreach (i, c: s[..^needed])
		{
			if (c == first && s[i:needed] == needle) return i;
		}
	}
	return SearchResult.MISSING?;
}

/**
 * Find the index of the last incidence of a string.
 *
 * @param [in] s
 * @param [in] needle
 * @pure
 * @ensure return < s.len
 * @require needle.len > 0 "The needle must be len 1 or more"
 * @return "the index of the needle"
 * @return! SearchResult.MISSING "if the needle cannot be found"
 **/
fn usz! String.rindex_of(s, String needle)
{
	usz needed = needle.len;
	if (needed > 0 && s.len >= needed)
	{
		char first = needle[0];
		foreach_r (i, c: s[..^needed])
		{
			if (c == first && s[i:needed] == needle) return i;
		}
	}
	return SearchResult.MISSING?;
}

fn String ZString.as_str(str)
{
	return (String)((char*)str)[:str.len()];
}

fn usz ZString.char_len(str)
{
	usz len = 0;
	char* ptr = (char*)str;
	while (char c = ptr++[0])
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

fn usz ZString.len(str)
{
	usz len = 0;
	char* ptr = (char*)str;
	while (char c = ptr++[0]) len++;
	return len;
}


fn ZString String.zstr_copy(s, Allocator* using = mem::heap())
{
	usz len = s.len;
	char* str = malloc(len + 1, .using = using);
	mem::copy(str, s.ptr, len);
	str[len] = 0;
	return (ZString)str;
}

fn String String.concat(s1, String s2, Allocator* using = mem::heap())
{
	usz full_len = s1.len + s2.len;
	char* str = malloc(full_len + 1, .using = using);
	usz s1_len = s1.len;
	mem::copy(str, s1.ptr, s1_len);
	mem::copy(str + s1_len, s2.ptr, s2.len);
	str[full_len] = 0;
	return (String)str[:full_len];
}

fn String String.tconcat(s1, String s2) => s1.concat(s2, mem::temp());


fn ZString String.zstr_tcopy(s) => s.zstr_copy(mem::temp()) @inline;

fn String String.copy(s, Allocator* using = mem::heap())
{
	usz len = s.len;
	char* str = malloc(len + 1, .using = using);
	mem::copy(str, s.ptr, len);
	str[len] = 0;
	return (String)str[:len];
}
fn void String.free(&s, Allocator* using = mem::heap())
{
	if (!s.len) return;
	mem::free(s.ptr, .using = using);
	*s = "";
}

fn String String.tcopy(s) => s.copy(mem::temp()) @inline;

fn String ZString.copy(z, Allocator* using = mem::heap()) => z.as_str().copy(using) @inline;
fn String ZString.tcopy(z) => z.as_str().copy(mem::temp()) @inline;

/**
 * Convert an UTF-8 string to UTF-16
 * @return "The UTF-16 string as a slice, allocated using the given allocator"
 * @return! UnicodeResult.INVALID_UTF8 "If the string contained an invalid UTF-8 sequence"
 * @return! AllocationFailure "If allocation of the string fails"
 **/
fn Char16[]! String.to_utf16(s, Allocator* using = mem::heap())
{
	usz len16 = conv::utf16len_for_utf8(s);
	Char16* data = malloc_checked(Char16, len16 + 1, .using = using)!;
	conv::utf8to16_unsafe(s, data)!;
	data[len16] = 0;
	return data[:len16];
}

fn WString! String.to_wstring(s, Allocator* using = mem::heap()) => (WString)s.to_utf16(using).ptr;

fn Char32[]! String.to_utf32(s, Allocator* using = mem::heap())
{
	usz codepoints = conv::utf8_codepoints(s);
	Char32* data = malloc_checked(Char32, codepoints + 1, .using = using)!;
	conv::utf8to32_unsafe(s, data)!;
	data[codepoints] = 0;
	return data[:codepoints];
}

fn void String.convert_ascii_to_lower(s)
{
	foreach (&c : s) if (c.is_upper()) *c += 'a' - 'A';
}

fn String String.ascii_to_lower(s, Allocator* using = mem::heap())
{
	String copy = s.copy(using);
	copy.convert_ascii_to_lower();
	return copy;
}

fn void String.convert_ascii_to_upper(s)
{
	foreach (&c : s) if (c.is_lower()) *c -= 'a' - 'A';
}

fn String String.ascii_to_upper(s, Allocator* using = mem::heap())
{
	String copy = s.copy(using);
	copy.convert_ascii_to_upper();
	return copy;
}

fn String! from_utf32(Char32[] utf32, Allocator* using = mem::heap())
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = malloc_checked(len + 1, .using = using)!;
	defer catch free(data, .using = using);
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return (String)data[:len];
}

fn String! from_utf16(Char16[] utf16, Allocator* using = mem::heap())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = malloc_checked(len + 1, .using = using)!;
	defer catch free(data, .using = using);
	conv::utf16to8_unsafe(utf16, data)!;
	data[len] = 0;
	return (String)data[:len];
}

fn String! from_wstring(WString wstring, Allocator* using = mem::heap())
{
	usz utf16_len;
	while (wstring[utf16_len] != 0) utf16_len++;
	Char16[] utf16 = wstring[:utf16_len];
	return from_utf16(utf16, using);
}

fn String! temp_from_wstring(WString wstring) => from_wstring(wstring) @inline;
fn String! temp_from_utf16(Char16[] utf16) => temp_from_utf16(utf16) @inline;

fn usz String.utf8_codepoints(s)
{
	usz len = 0;
	foreach (char c : s)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}


macro String.to_integer(string, $Type)
{
	usz len = string.len;
	usz index = 0;
	char* ptr = string.ptr;
	while (index < len && ascii::is_blank_m(ptr[index])) index++;
	if (len == index) return NumberConversion.EMPTY_STRING?;
	bool is_negative;
	switch (string[index])
	{
		case '-':
			if ($Type.min == 0) return NumberConversion.NEGATIVE_VALUE?;
			is_negative = true;
			index++;
		case '+':
			index++;
		default:
			break;
	}
	if (len == index) return NumberConversion.MALFORMED_INTEGER?;
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
		if (len == index) return NumberConversion.MALFORMED_INTEGER?;
	}
	$Type value = 0;
	while (index != len)
	{
		char c = {|
			char ch = string[index++];
			if (base != 16 || ch < 'A') return (char)(ch - '0');
			if (ch <= 'F') return (char)(ch - 'A');
			if (ch < 'a') return NumberConversion.MALFORMED_INTEGER?;
			if (ch > 'f') return NumberConversion.MALFORMED_INTEGER?;
			return (char)(ch - 'a');
		|}!;
		if (c >= base) return NumberConversion.MALFORMED_INTEGER?;
		value = {|
			if (is_negative)
			{
				$Type new_value = value * base - c;
				if (new_value > value) return NumberConversion.INTEGER_OVERFLOW?;
				return new_value;
			}
			$Type new_value = value * base + c;
			if (new_value < value) return NumberConversion.INTEGER_OVERFLOW?;
			return new_value;
		|}!;
	}
	return value;
}


fn Char16[]! String.to_temp_utf16(s) => s.to_utf16(mem::temp());
fn WString! String.to_temp_wstring(s) => s.to_wstring(mem::temp());

fn int128! String.to_int128(s) => s.to_integer(int128);
fn long! String.to_long(s) => s.to_integer(long);
fn int! String.to_int(s) => s.to_integer(int);
fn short! String.to_short(s) => s.to_integer(short);
fn ichar! String.to_ichar(s) => s.to_integer(ichar);

fn uint128! String.to_uint128(s) => s.to_integer(uint128);
fn ulong! String.to_ulong(s) => s.to_integer(ulong);
fn uint! String.to_uint(s) => s.to_integer(uint);
fn ushort! String.to_ushort(s) => s.to_integer(ushort);
fn char! String.to_uchar(s) => s.to_integer(char);

fn double! String.to_double(s) => s.to_real(double);
fn float! String.to_float(s) => s.to_real(float);
