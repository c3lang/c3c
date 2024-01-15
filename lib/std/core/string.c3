module std::core::string;
import std::ascii;

distinct ZString = inline char*;
distinct WString = inline Char16*;
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

macro String tformat(String fmt, ...)
{
	DString str = dstring::temp_with_capacity(fmt.len + $vacount * 8);
	str.appendf(fmt, $vasplat());
	return str.str_view();
}

macro String new_format(String fmt, ..., Allocator* allocator = allocator::heap())
{
	@pool(allocator)
	{
		DString str = dstring::temp_with_capacity(fmt.len + $vacount * 8);
		str.appendf(fmt, $vasplat());
		return str.copy_str(allocator);
	};
}


macro bool char_in_set(char c, String set)
{
	foreach (ch : set) if (ch == c) return true;
	return false;
}

fn String join_new(String[] s, String joiner, Allocator* allocator = allocator::heap())
{
	if (!s)
	{
		return (String)allocator::new_array(allocator, char, 2)[:0];
	}

	usz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	@pool(allocator)
	{
		DString res = dstring::temp_with_capacity(total_size);
		res.append(s[0]);
		foreach (String* &str : s[1..])
		{
			res.append(joiner);
			res.append(*str);
		}
		return res.copy_str(allocator);
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
 * @param [&inout] allocator "The allocator, defaults to the heap allocator"
 * @param max "Max number of elements, 0 means no limit, defaults to 0"
 * @require needle.len > 0 "The needle must be at least 1 character long"
 * @ensure return.len > 0
 **/
fn String[] String.split(s, String needle, usz max = 0, Allocator* allocator = allocator::heap())
{
	usz capacity = 16;
	usz i = 0;
	String* holder = allocator::alloc_array(allocator, String, capacity);
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
			holder = allocator::realloc(allocator, holder, String.sizeof * capacity);
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
	return s.split(needle, max, allocator::temp()) @inline;
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

fn String ZString.str_view(str)
{
	return (String)(str[:str.len()]);
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


fn ZString String.zstr_copy(s, Allocator* allocator = allocator::heap())
{
	usz len = s.len;
	char* str = allocator::malloc(allocator, len + 1);
	mem::copy(str, s.ptr, len);
	str[len] = 0;
	return (ZString)str;
}

fn String String.concat(s1, String s2, Allocator* allocator = allocator::heap())
{
	usz full_len = s1.len + s2.len;
	char* str = allocator::malloc(allocator, full_len + 1);
	usz s1_len = s1.len;
	mem::copy(str, s1.ptr, s1_len);
	mem::copy(str + s1_len, s2.ptr, s2.len);
	str[full_len] = 0;
	return (String)str[:full_len];
}

fn String String.tconcat(s1, String s2) => s1.concat(s2, allocator::temp());


fn ZString String.zstr_tcopy(s) => s.zstr_copy(allocator::temp()) @inline;

fn String String.copy(s, Allocator* allocator = allocator::heap())
{
	usz len = s.len;
	char* str = allocator::malloc(allocator, len + 1);
	mem::copy(str, s.ptr, len);
	str[len] = 0;
	return (String)str[:len];
}

fn void String.free(&s, Allocator* allocator = allocator::heap())
{
	if (!s.len) return;
	allocator::free(allocator, s.ptr);
	*s = "";
}

fn String String.tcopy(s) => s.copy(allocator::temp()) @inline;

fn String ZString.copy(z, Allocator* allocator = allocator::temp())
{
	return z.str_view().copy(allocator) @inline;
}

fn String ZString.tcopy(z)
{
	return z.str_view().copy(allocator::temp()) @inline;
}

/**
 * Convert an UTF-8 string to UTF-16
 * @return "The UTF-16 string as a slice, allocated using the given allocator"
 * @return! UnicodeResult.INVALID_UTF8 "If the string contained an invalid UTF-8 sequence"
 * @return! AllocationFailure "If allocation of the string fails"
 **/
fn Char16[]! String.to_new_utf16(s, Allocator* allocator = allocator::heap())
{
	usz len16 = conv::utf16len_for_utf8(s);
	Char16* data = allocator::alloc_array_try(allocator, Char16, len16 + 1)!;
	conv::utf8to16_unsafe(s, data)!;
	data[len16] = 0;
	return data[:len16];
}

/**
 * Convert an UTF-8 string to UTF-16
 * @return "The UTF-16 string as a slice, allocated using the given allocator"
 * @return! UnicodeResult.INVALID_UTF8 "If the string contained an invalid UTF-8 sequence"
 * @return! AllocationFailure "If allocation of the string fails"
 **/
fn Char16[]! String.to_temp_utf16(s)
{
	return s.to_new_utf16(allocator::temp());
}

fn WString! String.to_new_wstring(s, Allocator* allocator = allocator::heap())
{
	return (WString)s.to_new_utf16(allocator).ptr;
}

fn WString! String.to_temp_wstring(s)
{
	return (WString)s.to_temp_utf16().ptr;
}

fn Char32[]! String.to_new_utf32(s, Allocator* allocator = allocator::heap())
{
	usz codepoints = conv::utf8_codepoints(s);
	Char32* data = allocator::alloc_array_try(allocator, Char32, codepoints + 1)!;
	conv::utf8to32_unsafe(s, data)!;
	data[codepoints] = 0;
	return data[:codepoints];
}

fn Char32[]! String.to_temp_utf32(s)
{
	return s.to_new_utf32(allocator::temp());
}

fn void String.convert_ascii_to_lower(s)
{
	foreach (&c : s) if (c.is_upper()) *c += 'a' - 'A';
}

fn String String.new_ascii_to_lower(s, Allocator* allocator = allocator::heap())
{
	String copy = s.copy(allocator);
	copy.convert_ascii_to_lower();
	return copy;
}

fn String String.temp_ascii_to_lower(s, Allocator* allocator = allocator::heap())
{
	return s.new_ascii_to_lower(allocator::temp());
}

fn void String.convert_ascii_to_upper(s)
{
	foreach (&c : s) if (c.is_lower()) *c -= 'a' - 'A';
}

fn String String.new_ascii_to_upper(s, Allocator* allocator = allocator::heap())
{
	String copy = s.copy(allocator);
	copy.convert_ascii_to_upper();
	return copy;
}

fn StringIterator String.iterator(s)
{
	return { s, 0 };
}

fn String String.temp_ascii_to_upper(s)
{
	return s.new_ascii_to_upper(allocator::temp());
}

fn String! new_from_utf32(Char32[] utf32, Allocator* allocator = allocator::heap())
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = allocator::malloc_try(allocator, len + 1)!;
	defer catch allocator::free(allocator, data);
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return (String)data[:len];
}

fn String! new_from_utf16(Char16[] utf16, Allocator* allocator = allocator::heap())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = allocator::malloc_try(allocator, len + 1)!;
	defer catch allocator::free(allocator, data);
	conv::utf16to8_unsafe(utf16, data)!;
	data[len] = 0;
	return (String)data[:len];
}

fn String! new_from_wstring(WString wstring, Allocator* allocator = allocator::heap())
{
	usz utf16_len;
	while (wstring[utf16_len] != 0) utf16_len++;
	Char16[] utf16 = wstring[:utf16_len];
	return new_from_utf16(utf16, allocator);
}

fn String! temp_from_wstring(WString wstring) => new_from_wstring(wstring, allocator::temp()) @inline;
fn String! temp_from_utf16(Char16[] utf16) => new_from_utf16(utf16, allocator::temp()) @inline;

fn usz String.utf8_codepoints(s)
{
	usz len = 0;
	foreach (char c : s)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}


/**
 * @require (base <= 10 && base > 1) || base == 16 : "Unsupported base"
 **/
macro String.to_integer(string, $Type, int base = 10)
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
	$Type base_used = ($Type)base;
	if (string[index] == '0' && base == 10)
	{
		index++;
		if (index == len) return ($Type)0;
		switch (string[index])
		{
			case 'x':
			case 'X':
				base_used = 16;
				index++;
			case 'b':
			case 'B':
				base_used = 2;
				index++;
			case 'o':
			case 'O':
				base_used = 8;
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
			if (base_used != 16 || ch < 'A') return (char)(ch - '0');
			if (ch <= 'F') return (char)(ch - 'A' + 10);
			if (ch < 'a') return NumberConversion.MALFORMED_INTEGER?;
			if (ch > 'f') return NumberConversion.MALFORMED_INTEGER?;
			return (char)(ch - 'a' + 10);
		|}!;
		if (c >= base_used) return NumberConversion.MALFORMED_INTEGER?;
		value = {|
			if (is_negative)
			{
				$Type new_value = value * base_used - c;
				if (new_value > value) return NumberConversion.INTEGER_OVERFLOW?;
				return new_value;
			}
			$Type new_value = value * base_used + c;
			if (new_value < value) return NumberConversion.INTEGER_OVERFLOW?;
			return new_value;
		|}!;
	}
	return value;
}

fn int128! String.to_int128(s, int base = 10) => s.to_integer(int128, base);
fn long! String.to_long(s, int base = 10) => s.to_integer(long, base);
fn int! String.to_int(s, int base = 10) => s.to_integer(int, base);
fn short! String.to_short(s, int base = 10) => s.to_integer(short, base);
fn ichar! String.to_ichar(s, int base = 10) => s.to_integer(ichar, base);

fn uint128! String.to_uint128(s, int base = 10) => s.to_integer(uint128, base);
fn ulong! String.to_ulong(s, int base = 10) => s.to_integer(ulong, base);
fn uint! String.to_uint(s, int base = 10) => s.to_integer(uint, base);
fn ushort! String.to_ushort(s, int base = 10) => s.to_integer(ushort, base);
fn char! String.to_uchar(s, int base = 10) => s.to_integer(char, base);

fn double! String.to_double(s) => s.to_real(double);
fn float! String.to_float(s) => s.to_real(float);

fn Splitter String.splitter(self, String split)
{
	return Splitter { self, split, 0 };
}

struct Splitter
{
	String string;
	String split;
	usz current;
}

fn void Splitter.reset(&self)
{
	self.current = 0;
}

fn String! Splitter.next(&self)
{
	usz len = self.string.len;
	usz current = self.current;
	if (current >= len) return IteratorResult.NO_MORE_ELEMENT?;
	String remaining = self.string[current..];
	usz! next = remaining.index_of(self.split);
	if (try next)
	{
		defer self.current = current + next + self.split.len;
		return remaining[:next];
	}
	self.current = len;
	return remaining;
}


