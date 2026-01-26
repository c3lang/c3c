module std::core::string;
import std::io, std::ascii;


typedef String @if(!$defined(String)) = inline char[];
<*
 ZString is a pointer to a zero terminated array of chars.

 Use ZString when you need to interop with C zero terminated strings.
*>
typedef ZString = inline char*;

<*
 WString is a pointer to a zero terminated array of Char16.

 Depending on the platform, this may or may not correspond to wchar_t.
 For Windows, wchar_t is generally 16 bits, on MacOS it is 32 bits.
 However, for both MacOS and Linux, regular C strings (ZString)
 will be UTF-8 encoded, so there is no need to use the wchar_t versions
 of functions outside of encoding functions.
*>
typedef WString = inline Char16*;

<*
 Char32 is a UTF32 codepoint
*>
alias Char32 = uint;

<*
 Char16 is a UTF16 "character"
*>
alias Char16 = ushort;

<*
 Common faults used with strings
*>
faultdef INVALID_UTF8, INVALID_UTF16, CONVERSION_FAILED,
         EMPTY_STRING, NEGATIVE_VALUE, MALFORMED_INTEGER,
         INTEGER_OVERFLOW, MALFORMED_FLOAT, FLOAT_OUT_OF_RANGE;


<*
 Create a pointer to an UTF32 encoded string at compile time.

 @param $string : "The string to encode"
*>
macro Char32* @wstring32(String $string) @builtin
{
	return (Char32*)&&$$wstr32($string);
}

<*
 Create a slice of an UTF32 encoded string at compile time.

 @param $string : "The string to encode"
*>
macro Char32[] @char32(String $string) @builtin
{
	return $$wstr32($string)[..^2];
}

<*
 Create a WString (an UTF16 encoded string) at compile time.

 @param $string : "The string to encode"
*>
macro WString @wstring(String $string) @builtin
{
	return (WString)&&$$wstr16($string);
}

<*
 Create a slice of an UTF16 encoded string at compile time.

 @param $string : "The string to encode"
*>
macro Char16[] @char16(String $string) @builtin
{
	return $$wstr16($string)[..^2];
}

macro String @sprintf(String $format, ...) @builtin @const
{
	return $$sprintf($format, $vasplat);
}
<*
 Return a temporary ZString created using the formatting function.

 @param [in] fmt : `The formatting string`
*>
fn ZString tformat_zstr(String fmt, args...) @format(0)
{
	DString str = dstring::temp_with_capacity(fmt.len + args.len * 8);
	str.appendf(fmt, ...args);
	return str.zstr_view();
}

<*
 Return a new String created using the formatting function.

 @param [inout] allocator : `The allocator to use`
 @param [in] fmt : `The formatting string`
*>
fn String format(Allocator allocator, String fmt, args...) @format(1) => @pool()
{
	DString str = dstring::temp_with_capacity(fmt.len + args.len * 8);
	str.appendf(fmt, ...args);
	return str.copy_str(allocator);
}

<*
 Return a new String created using the formatting function, the resulting string must fit the buffer.

 @param [inout] buffer : `The buffer to use`
 @param [in] fmt : `The formatting string`
*>
fn String bformat(char[] buffer, String fmt, args...) @format(1)
{
	Formatter f;
	OutputFn format_fn = fn void?(void* buf, char c) {
		char[]* buffer_ref = buf;
		char[] buffer = *buffer_ref;
		if (buffer.len == 0) return io::BUFFER_EXCEEDED~;
		buffer[0] = c;
		*buffer_ref = buffer[1..];
	};
	char[] buffer_copy = buffer;
	f.init(format_fn, &buffer_copy);
	usz len = f.vprintf(fmt, args)!!;
	return (String)buffer[:len];
}

<*
 Return a temporary String created using the formatting function.

 @param [in] fmt : `The formatting string`
*>
fn String tformat(String fmt, args...) @format(0)
{
	DString str = dstring::temp_with_capacity(fmt.len + args.len * 8);
	str.appendf(fmt, ...args);
	return str.str_view();
}

<*
 Check if a character is in a set.

 @param c : `the character to check`
 @param [in] set : `String containing the characters`
 @pure
 @return `True if a character is in the set`
*>
macro bool char_in_set(char c, String set)
{
	foreach (ch : set) if (ch == c) return true;
	return false;
}

fn String join(Allocator allocator, String[] s, String joiner)
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
	@pool()
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

<*
 Replace all instances of one substring with a different string.

 @param [in] self
 @param [in] needle : `The string to be replaced`
 @param [in] new_str : `The replacement string`
 @param [&inout] allocator : `The allocator to use for the String`
 @return "The new string with the elements replaced"
*>
fn String String.replace(self, Allocator allocator, String needle, String new_str) @nodiscard => @pool()
{
	Splitter s = self.tokenize_all(needle);
	DString d;
	d.init(tmem, new_str.len * 2 + self.len + 16);
	(void)d.append(s.next());
	while (try element = s.next())
	{
		d.append(new_str);
		d.append(element);
	}
	return d.copy_str(allocator);
}

<*
 Replace all instances of one substring with a different string, allocating the new string on the temp allocator.

 @param [in] self
 @param [in] needle : `The string to be replaced`
 @param [in] new_str : `The replacement string`
 @return "The new string with the elements replaced"
*>
fn String String.treplace(self, String needle, String new_str)
{
	Splitter s = self.tokenize_all(needle);
	DString d;
	d.init(tmem, new_str.len * 2 + self.len + 16);
	(void)d.append(s.next());
	while (try element = s.next())
	{
		d.append(new_str);
		d.append(element);
	}
	return d.str_view();
}


<*
 Remove characters from the front and end of a string.

 @param [in] self : `The string to trim`
 @param [in] to_trim : `The set of characters to trim, defaults to whitespace`
 @pure
 @return `a substring of the string passed in`
*>
fn String String.trim(self, String to_trim = " \n\t\r\f\v")
{
    return self.trim_left(to_trim).trim_right(to_trim);
}

<*
 Remove characters from the front and end of a string.

 @param [in] self : `The string to trim`
 @param to_trim : `The set of characters to trim, defaults to whitespace`
 @pure
 @return `a substring of the string passed in`
*>
fn String String.trim_charset(self, AsciiCharset to_trim = ascii::WHITESPACE_SET)
{
	usz start = 0;
	usz len = self.len;
	while (start < len && to_trim.contains(self[start])) start++;
	while (len > start && to_trim.contains(self[len - 1])) len--;
    return self[start..len - 1];
}

<*
 Remove characters from the front of a string.

 @param [in] self : `The string to trim`
 @param [in] to_trim : `The set of characters to trim, defaults to whitespace`
 @pure
 @return `a substring of the string passed in`
*>
fn String String.trim_left(self, String to_trim = " \n\t\r\f\v")
{
	usz start = 0;
	usz len = self.len;
	while (start < len && char_in_set(self[start], to_trim)) start++;
	if (start == len) return self[:0];
	return self[start..];
}

<*
 Remove characters from the end of a string.

 @param [in] self : `The string to trim`
 @param [in] to_trim : `The set of characters to trim, defaults to whitespace`
 @pure
 @return `a substring of the string passed in`
*>
fn String String.trim_right(self, String to_trim = " \n\t\r\f\v")
{
	usz len = self.len;
	while (len > 0 && char_in_set(self[len - 1], to_trim)) len--;
	return self[:len];
}

<*
 Check if the String starts with the prefix.

 @param [in] self
 @param [in] prefix
 @pure
 @return `'true' if the string starts with the prefix`
*>
fn bool String.starts_with(self, String prefix)
{
	if (prefix.len > self.len) return false;
	if (!prefix.len) return true;
	return self[:prefix.len] == prefix;
}

<*
 Check if the String ends with the suffix.

 @param [in] self
 @param [in] suffix
 @pure
 @return `'true' if the string ends with the suffix`
*>
fn bool String.ends_with(self, String suffix)
{
	if (suffix.len > self.len) return false;
	if (!suffix.len) return true;
	return self[^suffix.len..] == suffix;
}

<*
 Strip the front of the string if the prefix exists.

 @param [in] self
 @param [in] prefix
 @pure
 @return `the substring with the prefix removed`
*>
fn String String.strip(self, String prefix)
{
	if (!prefix.len || !self.starts_with(prefix)) return self;
	return self[prefix.len..];
}

<*
 Strip the end of the string if the suffix exists.

 @param [in] self
 @param [in] suffix
 @pure
 @return `the substring with the suffix removed`
*>
fn String String.strip_end(self, String suffix)
{
	if (!suffix.len || !self.ends_with(suffix)) return self;
	// Note that this is the safe way if we want to support zero length.
	return self[:(self.len - suffix.len)];
}

<*
 Split a string into parts, e.g "a|b|c" split with "|" yields { "a", "b", "c" }

 @param [in] self
 @param [in] delimiter
 @param max : "Max number of elements, 0 means no limit, defaults to 0"
 @param skip_empty : "True to skip empty elements"
 @param [&inout] allocator : "The allocator to use for the String[]"

 @require delimiter.len > 0 : "The delimiter must be at least 1 character long"
 @ensure return.len > 0 || skip_empty
*>
fn String[] String.split(self, Allocator allocator, String delimiter, usz max = 0, bool skip_empty = false)
{
	usz capacity = 16;
	usz i = 0;
	String* holder = allocator::alloc_array(allocator, String, capacity);
	bool no_more = false;
	while (!no_more)
	{
		usz? index = i == max - 1 ? NOT_FOUND~ : self.index_of(delimiter);
		String res @noinit;
		if (try index)
		{
			res = self[:index];
			self = self[index + delimiter.len..];
		}
		else
		{
			res = self;
			no_more = true;
		}
		if (!res.len && skip_empty)
        {
        	continue;
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


<*
 This function is identical to String.split, but implicitly uses the
 temporary allocator.

 @param [in] s
 @param [in] delimiter
 @param max : "Max number of elements, 0 means no limit, defaults to 0"
 @param skip_empty : "True to skip empty elements"
*>
fn String[] String.tsplit(s, String delimiter, usz max = 0, bool skip_empty = false) => s.split(tmem, delimiter, max, skip_empty) @inline;

faultdef BUFFER_EXCEEDED;

<*
 Split a string into parts, e.g "a|b|c" split with "|" yields { "a", "b", "c" }

 @param [in] s
 @param [in] delimiter
 @param [inout] buffer
 @param max : "Max number of elements, 0 means no limit, defaults to 0"
 @require delimiter.len > 0 : "The delimiter must be at least 1 character long"
 @ensure return.len > 0 || skip_empty
 @return? BUFFER_EXCEEDED : `If there are more elements than would fit the buffer`
*>
fn String[]? String.split_to_buffer(s, String delimiter, String[] buffer, usz max = 0, bool skip_empty = false)
{
	usz max_capacity = buffer.len;
	usz i = 0;
	bool no_more = false;
	while (!no_more)
	{
		usz? index = i == max - 1 ? NOT_FOUND~ : s.index_of(delimiter);
		String res @noinit;
		if (try index)
		{
			res = s[:index];
			s = s[index + delimiter.len..];
		}
		else
		{
			res = s;
			no_more = true;
		}
		if (!res.len && skip_empty)
		{
			continue;
		}
		if (i == max_capacity)
		{
			return BUFFER_EXCEEDED~;
		}
		buffer[i++] = res;
	}
	return buffer[:i];
}

<*
 Check if a substring is found in the string.

 @param [in] s
 @param [in] substr : "The string to look for."
 @pure
 @return "true if the string contains the substring, false otherwise"
*>
fn bool String.contains(s, String substr)
{
	return @ok(s.index_of(substr));
}

<*
 Check if a character is found in the string.

 @param [in] s
 @param character : "The character to look for."
 @pure
 @return "true if the string contains the character, false otherwise"
*>
fn bool String.contains_char(s, char character)
{
	return @ok(s.index_of_char(character));
}

<*
 Check how many non-overlapping instances of a substring there is.

 If the substring has zero length, the number of matches is zero.

 @param [in] self : "The string to search"
 @param [in] substr : "The string to look for."
 @pure
 @return "The number of times matched"
*>
fn usz String.count(self, String substr)
{
	usz count = 0;
	usz needed = substr.len;
	if (needed == 0) return 0;
    char first = substr[0];
    while OUTER: (self.len >= needed)
    {
    	foreach (i, c: self[..^needed])
    	{
    		if (c == first && self[i : needed] == substr)
    		{
    		    count++;
    		    self = self[i + needed..];
    		    continue OUTER;
    		}
    	}
    	break;
    }
    return count;
}

<*
 Find the index of the first incidence of a string.

 @param [in] self
 @param character : "The character to look for"
 @pure
 @ensure return < self.len
 @return "the index of the character"
 @return? NOT_FOUND : "if the character cannot be found"
*>
fn usz? String.index_of_char(self, char character)
{
	foreach (i, c : self)
	{
		if (c == character) return i;
	}
	return NOT_FOUND~;
}

<*
 Find the index of the first incidence of a one of the chars.

 @param [in] self
 @param [in] characters : "The characters to look for"
 @pure
 @ensure return < self.len
 @return "the index of the character"
 @return? NOT_FOUND : "if the character cannot be found"
*>
fn usz? String.index_of_chars(String self, char[] characters)
{
    foreach (i, c : self)
    {
        foreach (j, pin : characters)
        {
            if (c == pin) return i;
        }
    }

    return NOT_FOUND~;
}

<*
 Find the index of the first incidence of a character.

 @param [in] self
 @param character : "The character to look for"
 @param start_index : "The index to start with, may exceed max index."
 @pure
 @ensure return < self.len
 @return "the index of the character"
 @return? NOT_FOUND : "if the character cannot be found starting from the start_index"
*>
fn usz? String.index_of_char_from(self, char character, usz start_index)
{
	usz len = self.len;
	if (len <= start_index) return NOT_FOUND~;
	for (usz i = start_index; i < len; i++)
	{
		if (self[i] == character) return i;
	}
	return NOT_FOUND~;
}

<*
 Find the index of the first incidence of a character starting from the end.

 @param [in] self
 @param character : "the character to find"
 @pure
 @ensure return < self.len
 @return "the index of the character"
 @return? NOT_FOUND : "if the character cannot be found"
*>
fn usz? String.rindex_of_char(self, char character)
{
	foreach_r (i, c : self)
	{
		if (c == character) return i;
	}
	return NOT_FOUND~;
}

<*
 Find the index of the first incidence of a string.

 @param [in] self
 @param [in] substr
 @pure
 @ensure return < self.len
 @require substr.len > 0 : "The string must be len 1 or more"
 @return "the index of the substring"
 @return? NOT_FOUND : "if the substring cannot be found"
*>
fn usz? String.index_of(self, String substr)
{
	usz needed = substr.len;
	if (needed > 0 && self.len >= needed)
	{
		char first = substr[0];
		foreach (i, c: self[..^needed])
		{
			if (c == first && self[i : needed] == substr) return i;
		}
	}
	return NOT_FOUND~;
}

<*
 Find the index of the last incidence of a string.

 @param [in] self
 @param [in] substr
 @pure
 @ensure return < self.len
 @require substr.len > 0 : "The substring must be len 1 or more"
 @return "the index of the substring"
 @return? NOT_FOUND : "if the substring cannot be found"
*>
fn usz? String.rindex_of(self, String substr)
{
	usz needed = substr.len;
	if (needed > 0 && self.len >= needed)
	{
		char first = substr[0];
		foreach_r (i, c: self[..^needed])
		{
			if (c == first && self[i : needed] == substr) return i;
		}
	}
	return NOT_FOUND~;
}

fn bool ZString.eq(self, ZString other) @operator(==)
{
	char* a = self;
	char* b = other;
	if (a == b) return true;
	if (!a || !b) return false;
	for (;; a++, b++)
	{
		char c = *a;
		if (c != *b) return false;
		if (!c) return true;
	}
}

fn String ZString.str_view(self)
{
	return (String)(self[:self.len()]);
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

fn usz ZString.len(self)
{
	usz len;
	for (char* ptr = (char*)self; *ptr; ptr++) len++;
	return len;
}

fn usz WString.len(self)
{
	usz len;
	for (Char16* ptr = (Char16*)self; *ptr; ptr++) len++;
	return len;
}

fn ZString String.zstr_copy(self, Allocator allocator)
{
	usz len = self.len;
	char* str = allocator::malloc(allocator, len + 1);
	mem::copy(str, self.ptr, len);
	str[len] = 0;
	return (ZString)str;
}

fn String String.concat(self, Allocator allocator, String s2)
{
	usz full_len = self.len + s2.len;
	char* str = allocator::malloc(allocator, full_len + 1);
	usz self_len = self.len;
	mem::copy(str, self.ptr, self_len);
	mem::copy(str + self_len, s2.ptr, s2.len);
	str[full_len] = 0;
	return (String)str[:full_len];
}

fn String String.tconcat(self, String s2) => self.concat(tmem, s2);


fn ZString String.zstr_tcopy(self) => self.zstr_copy(tmem) @inline;

<*
 Copy this string, by duplicating the string, always adding a zero byte
 sentinel, so that it safely can be converted to a ZString by a
 cast.
*>
fn String String.copy(self, Allocator allocator)
{
	usz len = self.len;
	char* str = allocator::malloc(allocator, len + 1);
	mem::copy(str, self.ptr, len);
	str[len] = 0;
	return (String)str[:len];
}

fn void String.free(&self, Allocator allocator)
{
	if (!self.ptr) return;
	allocator::free(allocator, self.ptr);
	*self = "";
}

fn String String.tcopy(self) => self.copy(tmem) @inline;

fn String ZString.copy(self, Allocator allocator)
{
	return self.str_view().copy(allocator) @inline;
}

fn String ZString.tcopy(self)
{
	return self.str_view().copy(tmem) @inline;
}

<*
 Convert an UTF-8 string to UTF-16
 @return "The UTF-16 string as a slice, allocated using the given allocator"
 @return? INVALID_UTF8 : "If the string contained an invalid UTF-8 sequence"
*>
fn Char16[]? String.to_utf16(self, Allocator allocator)
{
	usz len16 = conv::utf16len_for_utf8(self);
	Char16* data = allocator::alloc_array_try(allocator, Char16, len16 + 1)!;
	conv::utf8to16_unsafe(self, data)!;
	data[len16] = 0;
	return data[:len16];
}

fn Char16[]? String.to_temp_utf16(self) => self.to_utf16(tmem);

fn WString? String.to_wstring(self, Allocator allocator)
{
	return (WString)self.to_utf16(allocator).ptr;
}

fn WString? String.to_temp_wstring(self) => self.to_wstring(tmem);

fn Char32[]? String.to_utf32(self, Allocator allocator)
{
	usz codepoints = conv::utf8_codepoints(self);
	Char32* data = allocator::alloc_array_try(allocator, Char32, codepoints + 1)!;
	conv::utf8to32_unsafe(self, data)!;
	data[codepoints] = 0;
	return data[:codepoints];
}

fn Char32[]? String.to_temp_utf32(self) => self.to_utf32(tmem);

<*
 Convert a string to ASCII lower case in place.

 @param [inout] self
 @pure
*>
fn void String.convert_to_lower(self)
{
	foreach (&c : self) if (c.is_upper() @pure) *c += 'a' - 'A';
}

fn String String.to_lower_copy(self, Allocator allocator)
{
	String copy = self.copy(allocator);
	copy.convert_to_lower();
	return copy;
}

fn String String.to_lower_tcopy(self)
{
	return self.to_lower_copy(tmem);
}

<*
 Convert a string to ASCII upper case.

 @param [inout] self
 @pure
*>
fn void String.convert_to_upper(self)
{
	foreach (&c : self) if (c.is_lower() @pure) *c -= 'a' - 'A';
}

<*
 Returns a string converted to ASCII upper case.

 @param [in] self
 @param [inout] allocator

 @return `a new String converted to ASCII upper case.`
*>
fn String String.to_upper_copy(self, Allocator allocator)
{
	String copy = self.copy(allocator);
	copy.convert_to_upper();
	return copy;
}

fn String String.capitalize_copy(self, Allocator allocator)
{
	String s = self.copy(allocator);
	if (s.len > 0 && s[0].is_lower())
	{
		s[0] &= (char)~0x20;
	}
	return s;
}

<*
 Convert a string from `snake_case` to PascalCase.

 @param [in] self
 @return `"FooBar" from "foo_bar" the resulting pointer may safely be cast to ZString.`
*>
fn String String.snake_to_pascal_copy(self, Allocator allocator)
{
	Splitter splitter = self.tokenize("_");
	char[] new_string = allocator::alloc_array(allocator, char, self.len + 1);
	usz index = 0;
	while (try s = splitter.next())
	{
		assert(s.len > 0);
		char c = s[0];
		if (c.is_lower()) c = c.to_upper();
		new_string[index++] = c;
		s = s[1..];
		new_string[index:s.len] = s[..];
		index += s.len;
	}
	new_string[index] = 0;
	return (String)new_string[:index];
}

<*
 Movifies the current string from `snake_case` to PascalCase.

 @param [inout] self
*>
fn void String.convert_snake_to_pascal(&self)
{
	Splitter splitter = self.tokenize("_");
	String new_string = *self;
	usz index = 0;
	while (try s = splitter.next())
	{
		assert(s.len > 0);
		char c = s[0];
		if (c.is_lower()) c = c.to_upper();
		new_string[index++] = c;
		s = s[1..];
		new_string[index:s.len] = s[..];
		index += s.len;
	}
	*self = new_string[:index];
}

<*
 Convert a string from `PascalCase` to `snake_case`.

 @param [in] self
 @return `"foo_bar" from "FooBar" the resulting pointer may safely be cast to ZString.`
*>
fn String String.pascal_to_snake_copy(self, Allocator allocator) => @pool()
{
	DString d;
	d.init(tmem, (usz)(self.len * 1.5));
	usz index = 0;
	foreach (i, c : self)
	{
		if (c.is_upper())
		{
			if (i > 0 && ((self[i - 1].is_lower() || self[i - 1].is_digit()) || (i < self.len - 1 && self[i + 1].is_lower())))
            {
                d.append_char('_');
            }
			d.append_char(c.to_lower());
			continue;
		}
		d.append_char(c);
	}
	return d.copy_str(allocator);
}


fn StringIterator String.iterator(self)
{
	return { self, 0 };
}

<*
 @param [in] self
 @return `a temporary String converted to ASCII upper case.`
*>
fn String String.to_upper_tcopy(self)
{
	return self.to_upper_copy(tmem);
}

fn String? from_utf32(Allocator allocator, Char32[] utf32)
{
	usz len = conv::utf8len_for_utf32(utf32);
	char* data = allocator::malloc_try(allocator, len + 1)!;
	defer catch allocator::free(allocator, data);
	conv::utf32to8_unsafe(utf32, data);
	data[len] = 0;
	return (String)data[:len];
}

fn String? from_utf16(Allocator allocator, Char16[] utf16)
{
	usz len = conv::utf8len_for_utf16(utf16);
	char* data = allocator::malloc_try(allocator, len + 1)!;
	defer catch allocator::free(allocator, data);
	conv::utf16to8_unsafe(utf16, data)!;
	data[len] = 0;
	return (String)data[:len];
}

fn String? from_wstring(Allocator allocator, WString wstring)
{
	usz utf16_len;
	while (wstring[utf16_len] != 0) utf16_len++;
	Char16[] utf16 = wstring[:utf16_len];
	return from_utf16(allocator, utf16);
}

fn String? tfrom_wstring(WString wstring) => from_wstring(tmem, wstring) @inline;
fn String? tfrom_utf16(Char16[] utf16) => from_utf16(tmem, utf16) @inline;

fn usz String.utf8_codepoints(s)
{
	usz len = 0;
	foreach (char c : s)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

<*
 Determine whether the current string actually points to a ZString-like string.

 This is done by looking at the byte one step after the end of the string. If this
 is zero, it is considered zero terminated.

 This function can safely be used with data pointing to null. However, it will not
 work correctly if the pointer is invalid, for example it is already freed.
*>
fn bool String.is_zstr(self) @deprecated("Unsafe, use copy instead")
{
	return self.ptr && *(self.ptr + self.len) == 0;
}

<*
 Return a pointer to the string *iff* it is a pointer
 to a zero terminated string, otherwise return a temp allocated zstring copy.

 This function is suitable if you are converting strings to ZString on the temp
 allocator, but suspect that the String might actually already point to zero
 terminated data.

 The function looks one step beyond the end of the slice to determine this,
 which means that if that data is then modified after this call, this function
 might behave incorrectly.

 For this reason, try to ensure that the resulting ZString is immediately used.

 @ensure return[self.len] == 0
*>
fn ZString String.quick_zstr(self) @deprecated("Unsafe, use zstr_tcopy instead")
{
	return self.zstr_tcopy();
}


<*
 Convert a number to a given base. If the base is not given, then
 it will be inferred from the number if the string starts with 0x 0o or 0b and the
 base is given as 10.

 Furthermore it will skip any spaces before and after the number.

 @param $Type : "The type to convert to"
 @param base : "The base to convert to"
 @require base > 0 && base <= 16 : "Unsupported base"
 @return? MALFORMED_INTEGER : "When the value has some illegal character"
 @return? INTEGER_OVERFLOW : "If the value does not fit in the given type"
 @return? EMPTY_STRING : "If the string was empty"
 @return? NEGATIVE_VALUE : "If the type was unsigned, and the value had a - prefix"
*>
macro String.to_integer(self, $Type, int base = 10)
{
	usz len = self.len;
	usz index = 0;
	char* ptr = self.ptr;
	while (index < len && ptr[index].is_blank()) index++;
	if (len == index) return EMPTY_STRING~;
	bool is_negative;
	switch (self[index])
	{
		case '-':
			if ($Type.min == 0) return NEGATIVE_VALUE~;
			is_negative = true;
			index++;
		case '+':
			index++;
		default:
			break;
	}
	if (len == index) return MALFORMED_INTEGER~;
	$Type base_used = ($Type)base;
	if (self[index] == '0' && base == 10)
	{
		index++;
		if (index == len) return ($Type)0;
		switch (self[index])
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
		if (len == index) return MALFORMED_INTEGER~;
	}
	$Type value = 0;
	while (index != len)
	{
		char c = self[index++];
		switch
		{
			case base_used < 10 || c < 'A':   c -= '0';
			case c <= 'F':                    c -= 'A' - 10;
			case c < 'a' || c > 'f':          return MALFORMED_INTEGER~;
			default:                          c -= 'a' - 10;
		}
		if (c >= base_used) return MALFORMED_INTEGER~;
		do
		{
			if (is_negative)
			{
				value = value.overflow_mul(base_used).overflow_sub(c) ?? INTEGER_OVERFLOW~!;
				break;
			}
			value = value.overflow_mul(base_used).overflow_add(c) ?? INTEGER_OVERFLOW~!;
		};
	}
	return value;
}

fn int128? String.to_int128(self, int base = 10) => self.to_integer(int128, base);
fn long? String.to_long(self, int base = 10) => self.to_integer(long, base);
fn int? String.to_int(self, int base = 10) => self.to_integer(int, base);
fn short? String.to_short(self, int base = 10) => self.to_integer(short, base);
fn ichar? String.to_ichar(self, int base = 10) => self.to_integer(ichar, base);

fn uint128? String.to_uint128(self, int base = 10) => self.to_integer(uint128, base);
fn ulong? String.to_ulong(self, int base = 10) => self.to_integer(ulong, base);
fn uint? String.to_uint(self, int base = 10) => self.to_integer(uint, base);
fn ushort? String.to_ushort(self, int base = 10) => self.to_integer(ushort, base);
fn char? String.to_uchar(self, int base = 10) => self.to_integer(char, base);

fn double? String.to_double(self) => self.to_real(double);
fn float? String.to_float(self) => self.to_real(float);


<*
 Create a Splitter to track tokenizing of a string. Tokenize will turn "foo:bar::baz" into
 "foo", "bar" and "baz", if you want the empty string to be present, use `tokenize_all`
 instead.

 @param [in] split : "The string to use for splitting"
 @return "A Splitter to track the state"
*>
fn Splitter String.tokenize(self, String split)
{
	return { .string = self, .split = split, .type = TOKENIZE };
}

<*
 Create a Splitter to track tokenizing of a string. Tokenize will turn "foo:bar::baz" into
 "foo", "bar" and "baz", if you want the empty string to be present, use `tokenize_all`
 instead.

 @param [in] split : "The string to use for splitting"
 @param skip_last : "Set to true to not include the last empty token if present (default: false)"
 @return "A Splitter to track the state"
*>
fn Splitter String.tokenize_all(self, String split, bool skip_last = false)
{
	return {
		.string = self,
		.split = split,
		.type = skip_last ? TOKENIZE_ALL_SKIP_LAST : TOKENIZE_ALL
	};
}

fn Splitter String.splitter(self, String split) @deprecated("Use tokenize_all instead")
{
	return self.tokenize_all(split, skip_last: true);
}

<*
 This macro will create a string description of a struct.

 @param [&inout] allocator : "The allocator to use"
 @param x : "The struct to create a description of"
*>
macro String from_struct(Allocator allocator, x)
{
	DString s;
	@stack_mem(512; Allocator mem)
	{
		s.init(allocator: mem);
		io::fprint(&s, x)!!;
		return s.copy_str(allocator);
	};
}

<*
 This macro will create a temporary string description of a struct.

 @param x : "The struct to create a description of"
*>
macro String tfrom_struct(x) => from_struct(tmem, x);

const uint SURROGATE_OFFSET @private = 0x10000;
const uint SURROGATE_GENERIC_MASK @private = 0xF800;
const uint SURROGATE_MASK @private = 0xFC00;
const uint SURROGATE_CODEPOINT_MASK @private = 0x03FF;
const uint SURROGATE_BITS @private = 10;
const uint SURROGATE_LOW_VALUE @private = 0xDC00;
const uint SURROGATE_HIGH_VALUE @private = 0xD800;

enum SplitterType
{
	TOKENIZE,
	TOKENIZE_ALL,
	TOKENIZE_ALL_SKIP_LAST
}

<*
 Splitter is handles tokenizing strings.
*>
struct Splitter
{
	String string;
	String split;
	usz current;
	SplitterType type;
	int last_index;
}

fn void Splitter.reset(&self)
{
	self.current = 0;
}

fn bool Splitter.at_end(&self)
{
	return self.current > self.string.len;
}

fn String? Splitter.next(&self)
{
	while (true)
	{
		usz len = self.string.len;
		usz current = self.current;
		if (current > len) return NO_MORE_ELEMENT~;
		if (current == len)
		{
			if (self.type != TOKENIZE_ALL) return NO_MORE_ELEMENT~;
			self.current++;
			return self.string[current - 1:0];
		}
		String remaining = self.string[current..];
		usz? next = remaining.index_of(self.split);
		if (try next)
		{
			self.current = current + next + self.split.len;
			if (!next && self.type == TOKENIZE) continue;
			return remaining[:next];
		}
		self.current = len + 1;
		return remaining;
	}
}
