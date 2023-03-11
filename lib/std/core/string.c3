module std::core::string;

typedef ZString = distinct inline char*;
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

macro String printf(String fmt, ..., Allocator* using = mem::heap())
{
	@allocating_pool(using; bool is_temp)
	{
		DString str;
		str.tinit();
		str.printf(fmt, $vasplat());
		return using == is_temp ? str.str() : str.copy_str(using);
	};
}

macro String tprintf(String fmt, ...)
{
	DString str;
	str.tinit();
	str.printf(fmt, $vasplat());
	return str.str();
}

macro bool char_in_set(char c, String set)
{
	foreach (ch : set) if (ch == c) return true;
	return false;
}

/**
 * @param [in] string
 * @param [in] to_trim
 **/
fn String String.trim(String string, String to_trim = "\t\n\r ")
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
fn bool String.starts_with(String string, String needle)
{
	if (needle.len > string.len) return false;
	if (!needle.len) return true;
	return string[:needle.len] == needle;
}

/**
 * @param [in] string
 * @param [in] needle
 **/
fn bool String.ends_with(String string, String needle)
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
fn String String.strip(String string, String needle)
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
fn String String.strip_end(String string, String needle)
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
fn String[] String.split(String s, String needle, usz max = 0, Allocator* using = mem::heap())
{
	usz capacity = 16;
	usz i = 0;
	String* holder = malloc(String, capacity, .using = using);
	bool no_more = false;
	while (!no_more)
	{
		usz! index = i == max - 1 ? SearchResult.MISSING! : s.index_of(needle);
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
fn String[] String.tsplit(String s, String needle, usz max = 0)
{
	return s.split(needle, max, mem::temp()) @inline;
}

/**
 * Find the index of the first incidence of a string.
 *
 * @param [in] s
 * @param [in] needle
 * @pure
 * @ensure return < s.len
 * @require needle.len > 0 "The needle must be len 1 or more"
 **/
fn usz! String.index_of(String s, String needle)
{
	usz match = 0;
	usz needed = needle.len;
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

/**
 * Find the index of the last incidence of a string.
 *
 * @param [in] s
 * @param [in] needle
 * @pure
 * @ensure return < s.len
 * @require needle.len > 0 "The needle must be len 1 or more"
 **/
fn usz! String.rindex_of(String s, String needle)
{
	usz match = 0;
	usz needed = needle.len;
	usz index_start = 0;
	char search = needle[^1];
	foreach_r (usz i, char c : s)
	{
		if (c == search)
		{
			if (!match) index_start = i;
			match++;
			if (match == needed) return index_start - needle.len + 1;
			search = needle[^(match + 1)];
			continue;
		}
		if (match)
		{
			match = 0;
			search = needle[^1];
		}
	}
	return SearchResult.MISSING!;
}

fn ZString String.zstr_copy(String s, Allocator* using = mem::heap())
{
    usz len = s.len;
    char* str = malloc(len + 1, .using = using);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (ZString)str;
}

fn String String.concat(String s1, String s2, Allocator* using = mem::heap())
{
    usz full_len = s1.len + s2.len;
    char* str = malloc(full_len + 1, .using = using);
    usz s1_len = s1.len;
    mem::copy(str, s1.ptr, s1_len);
    mem::copy(str + s1_len, s2.ptr, s2.len);
    str[full_len] = 0;
    return (String)str[:full_len];
}

fn String String.tconcat(String s1, String s2) => s1.concat(s2, mem::temp());


fn ZString String.zstr_tcopy(String s) => s.zstr_copy(mem::temp()) @inline;

fn String String.copy(String s, Allocator* using = mem::heap())
{
    usz len = s.len;
    char* str = malloc(len + 1, .using = using);
    mem::copy(str, s.ptr, len);
    str[len] = 0;
    return (String)str[:len];
}

fn String String.tcopy(String s) => s.copy(mem::temp()) @inline;

fn String ZString.copy(ZString z, Allocator* using = mem::heap()) => z.as_str().copy(using) @inline;
fn String ZString.tcopy(ZString z) => z.as_str().copy(mem::temp()) @inline;

fn Char16[]! String.to_utf16(String s, Allocator* using = mem::heap())
{
	usz len16 = conv::utf16len_for_utf8(s);
	Char16* data = malloc_checked(Char16, len16 + 1, .using = using)?;
	conv::utf8to16_unsafe(s, data)?;
	data[len16] = 0;
	return data[:len16];
}

fn String! from_utf16(Char16[] utf16, Allocator* using = mem::heap())
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = malloc_checked(len + 1, .using = using)?;
	conv::utf16to8_unsafe(utf16, data)?;
	data[len] = 0;
	return (String)data[:len];
}


fn Char16[]! String.to_temp_utf16(String s) => s.to_utf16(mem::temp());
