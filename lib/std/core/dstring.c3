module std::core::dstring;
import std::io, std::math;

<*
 The DString offers a dynamic string builder: an owning, growable, allocator-backed buffer of
 characters. A zero-initialized DString is valid and lazily initializes itself (using the temp
 allocator) on first use.
*>
typedef DString (OutStream) = DStringOpaque*;
typedef DStringOpaque = void;

<* The smallest capacity a DString will ever be initialized or grown to. *>
const sz MIN_CAPACITY @private = 16;

<*
 Initialize the DString with a particular allocator.

 @param [&inout] allocator : "The allocator to use"
 @param capacity : "Starting capacity, defaults to MIN_CAPACITY and cannot be smaller"
 @return "Return the DString itself"
 @require !self.data() : "String already initialized"
*>
fn DString DString.init(&self, Allocator allocator, sz capacity = MIN_CAPACITY)
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = alloc::alloc_with_padding(allocator, StringData, capacity)!!;
	data.allocator = allocator;
	data.len = 0;
	data.capacity = capacity;
	return *self = (DString)data;
}

<*
 Initialize the DString with the temp allocator. Note that if the dstring is never
 initialized, this is the allocator it will default to.

 @param capacity : "Starting capacity, defaults to MIN_CAPACITY and cannot be smaller"
 @return "Return the DString itself"
 @require !self.data() : "String already initialized"
*>
fn DString DString.tinit(&self, sz capacity = MIN_CAPACITY)
{
	return self.init(tmem, capacity) @inline;
}

<*
 Create a new, empty DString with at least the given starting capacity.

 @param [&inout] allocator : "The allocator to use"
 @param capacity : "Starting capacity, defaults to MIN_CAPACITY and cannot be smaller"
 @return "The newly created DString"

 @require [capacity] capacity >= 0 : "Capacity must be positive"
*>
fn DString new_with_capacity(Allocator allocator, sz capacity)
{
	return (DString){}.init(allocator, capacity);
}

<*
 Create a new, empty DString with at least the given starting capacity, using the temp allocator.

 @param capacity : "Starting capacity, defaults to MIN_CAPACITY and cannot be smaller"
 @return "The newly created DString"

 @require [capacity] capacity >= 0 : "Capacity must be positive"
*>
fn DString temp_with_capacity(sz capacity) => new_with_capacity(tmem, capacity) @inline;

<*
 Create a new DString, initialized with a copy of `c`.

 @param [&inout] allocator : "The allocator to use"
 @param [in] c : "The initial contents of the DString"
 @return "The newly created DString"
*>
fn DString new(Allocator allocator, String c = "")
{
	sz len = c.len;
	StringData* data = (StringData*)new_with_capacity(allocator, len);
	if (len)
	{
		data.len = len;
		mem::copy(&data.chars, c.ptr, len);
	}
	return (DString)data;
}

<*
 Create a new DString, initialized with a copy of `s`, using the temp allocator.

 @param [in] s : "The initial contents of the DString"
 @return "The newly created DString"
*>
fn DString temp(String s = "") => new(tmem, s) @inline;


<*
 Replace every occurrence of `ch` with `replacement` in place. A no-op on an uninitialized
 DString.

 @param ch : "The character to replace"
 @param replacement : "The character to replace it with"
*>
fn void DString.replace_char(self, char ch, char replacement)
{
	StringData* data = self.data();
	if (!data) return;
	foreach (&c : data.chars[:data.len])
	{
		if (*c == ch) *c = replacement;
	}
}

<*
 Replace every occurrence of `needle` with `replacement` in place. If `needle` is empty,
 `replacement` is inserted before, between, and after every character. A no-op on an
 uninitialized DString unless `needle` is also empty, in which case `replacement` is appended.

 @param [in] needle : "The substring to search for"
 @param [in] replacement : "The string to replace each match with"
*>
fn void DString.replace(&self, String needle, String replacement)
{
	StringData* data = self.data();
	sz needle_len = needle.len;
	if (!data)
	{
		if (needle_len > 0) return;
		if (!replacement) return;
		self.append(replacement);
		return;
	}
	if (data.len < needle_len) return;
	sz replace_len = replacement.len;
	if (needle_len == 1 && replace_len == 1)
	{
		self.replace_char(needle[0], replacement[0]);
		return;
	}
	if (needle_len == 0)
	{
		if (!replace_len) return;
		@pool()
		{
			String str = self.tcopy_str();
			self.clear();
			self.append_string(replacement);
			foreach (i, c : str)
			{
				self.append_char(c);
				self.append_string(replacement);
			}
		};
		return;
	}
	@pool()
	{
		String str = self.tcopy_str();
		self.clear();
		sz len = str.len;
		sz match = 0;
		sz run_start = 0;
		for (sz i = 0; i < len; i++)
		{
			if (str[i] == needle[match])
			{
				match++;
				if (match == needle_len)
				{
					sz match_start = i - needle_len + 1;
					if (match_start > run_start) self.append_string(str[run_start:match_start - run_start]);
					self.append_string(replacement);
					run_start = i + 1;
					match = 0;
				}
				continue;
			}
			if (match > 0)
			{
				i -= match;
				match = 0;
			}
		}
		if (run_start < len) self.append_string(str[run_start..]);
	};
}

<*
 Concatenate `self` and `b` into a newly allocated DString.

 @param [&inout] allocator : "The allocator to use for the new DString"
 @param [in] b : "The DString to append after self"
 @return "A new DString containing self's contents followed by b's contents"
*>
fn DString DString.concat(self, Allocator allocator, DString b) @nodiscard
{
	DString string;
	string.init(allocator, self.len() + b.len());
	string.append(self);
	string.append(b);
	return string;
}

<*
 Concatenate `self` and `b` into a newly allocated DString, using the temp allocator.

 @param [in] b : "The DString to append after self"
 @return "A new DString containing self's contents followed by b's contents"
*>
fn DString DString.tconcat(self, DString b) => self.concat(tmem, b);

<*
 Return a null-terminated view of the DString's contents. The returned pointer is only valid
 as long as the DString isn't mutated or freed; a null pointer is never returned (an
 uninitialized DString yields an empty, null-terminated string).

 @return "A null-terminated view of the current contents"
*>
fn ZString DString.zstr_view(&self)
{
	StringData* data = self.data();
	if (!data) return "";
	if (data.capacity == data.len)
	{
		_reserve_inline(self, 1);
		data = self.data();
		data.chars[data.len] = 0;
	}
	else if (data.chars[data.len] != 0)
	{
		data.chars[data.len] = 0;
	}
	return (ZString)&data.chars[0];
}

<*
 @return "The current capacity in bytes, or 0 if uninitialized"
*>
fn sz DString.capacity(self)
{
	if (!self) return 0;
	return self.data().capacity;
}

<*
 @return "The current length in bytes, or 0 if uninitialized"
*>
fn sz DString.len(&self) @operator(len)
{
	if (!*self) return 0;
	return self.data().len;
}

<*
 Truncate (or, degenerately, extend without initializing) the DString to `new_size` bytes.
 A no-op on an uninitialized DString.

 @param new_size : "The new length in bytes"
 @require [new_size] (usz)new_size <= (usz)self.len() : "new_size must be between 0 and self.len()"
*>
fn void DString.chop(self, sz new_size)
{
	if (!self) return;
	self.data().len = new_size;
}

<*
 @return "A view of the current contents, or an empty string if uninitialized"
*>
fn String DString.str_view(self)
{
	if (!self) return "";
	StringData* data = self.data();
	return (String)data.chars[:data.len];
}

<*
 Get the character at `index`.

 @param index : "The index to read"
 @return "The character at that index"
 @require [index] (usz)index < (usz)self.len() : "index must be between 0 and self.len()"
 @require self.data() != null : "Empty string"
*>
fn char DString.char_at(self, sz index) @operator([])
{
	return self.data().chars[index];
}

<*
 Get a pointer to the character at `index`. The pointer is only valid until the next mutation.

 @param index : "The index to reference"
 @return "A pointer to the character at that index"

 @require [index] (usz)index < (usz)self.len() : "index must be between 0 and self.len()"
 @require self.data() != null  : "Empty string"
*>
fn char* DString.char_ref(&self, sz index) @operator(&[])
{
	return &self.data().chars[index];
}

<*
 Append a sequence of UTF32 codepoints, encoding each as UTF8.

 @param chars : "The codepoints to append"
 @return "The number of bytes appended"
*>
fn sz DString.append_utf32(&self, Char32[] chars)
{
	self.reserve(chars.len);
	sz end = self.len();
	foreach (Char32 c : chars)
	{
		self.append_char32(c);
	}
	return self.data().len - end;
}

<*
 Set the character at `index`.

 @param index : "The index to write"
 @param c : "The new character"

 @require [index] (usz)index < (usz)self.len() : "index must be between 0 and self.len()"
*>
fn void DString.set(self, sz index, char c) @operator([]=)
{
	self.data().chars[index] = c;
}

<*
 Append `c` `times` times.

 @param c : "The character to append"
 @param times : "How many times to append it"

 @require [times] times >= 0 : "Repetition may not be negative"
*>
fn void DString.append_char_repeat(&self, char c, sz times)
{
	if (times < 1) return;
	self.reserve(times);
	StringData* data = self.data();
	for (sz i = 0; i < times; i++)
	{
		data.chars[data.len++] = c;
	}
}

<*
 Append `c` `times` times. Dispatches to a specialized implementation for `String` and `char`,
 falling back to repeated calls to `append` for any other appendable type.

 @param c : "The value to append"
 @param times : "How many times to append it"

 @require [times] times >= 0 : "Repetition may not be negative"
*>
macro void DString.append_repeat(&self, c, sz times)
{
	if (times < 1) return;
	$switch $Typeof(c):
		$case String:
			self.append_string_repeat(c, times);
		$case char:
			self.append_char_repeat(c, times);
		$default:
			for (sz i = 0; i < times; i++)
			{
				self.append(c);
			}
	$endswitch
}

<*
 Append `c` `times` times.

 @param c : "The string to append"
 @param times : "How many times to append it"

 @require [times] times >= 0 : "Negative repeat is not allowed"
 @require [times] times == 0 || sz::max / times > c.len : "Resulting string is too large"
*>
fn void DString.append_string_repeat(&self, String c, sz times)
{
	sz strlen = c.len;
	if (!times || !strlen) return;
	self.reserve(times * strlen);
	StringData* data = self.data();
	sz len = data.len;
	for (sz i = 0; i < times; i++, len += strlen)
	{
		mem::copy(&data.chars[len], c, strlen);
	}
	data.len = len;
}

<*
 Append a single UTF32 codepoint, encoded as UTF8.

 @param c : "The codepoint to append"
 @return "The number of bytes appended (1-4)"

 @require [c] c <= 0x10ffff
*>
fn sz DString.append_char32(&self, Char32 c)
{
	char[4] buffer @noinit;
	char* p = &buffer;
	sz n = conv::char32_to_utf8_unsafe(c, &p);
	self.reserve(n);
	StringData* data = self.data();
	data.chars[data.len:n] = buffer[:n];
	data.len += n;
	return n;
}

<*
 Clone this DString into a new one using the temp allocator.

 @return "A new, independently-allocated copy"
*>
fn DString DString.tcopy(&self) => self.copy(tmem);

<*
 Clone this DString into a new one.

 @param [&inout] allocator : "The allocator to use for the copy"
 @return "A new, independently-allocated copy"
*>
fn DString DString.copy(self, Allocator allocator) @nodiscard
{
	if (!self) return new(allocator);
	StringData* data = self.data();
	DString new_string = new_with_capacity(allocator, data.capacity);
	mem::copy((char*)new_string.data(), (char*)data, StringData::size + data.len);
	new_string.data().allocator = allocator;
	return new_string;
}

<*
 Copy the contents into a newly allocated, null-terminated ZString.

 @param [&inout] allocator : "The allocator to use"
 @return "A newly allocated, null-terminated copy of the contents"
*>
fn ZString DString.copy_zstr(self, Allocator allocator) @nodiscard
{
	sz str_len = self.len();
	if (!str_len)
	{
		return (ZString)alloc::calloc(allocator, 1);
	}
	char* zstr = alloc::malloc(allocator, str_len + 1);
	StringData* data = self.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

<*
 Copy the contents into a newly allocated String.

 @param [&inout] allocator : "The allocator to use"
 @return "A newly allocated copy of the contents"
*>
fn String DString.copy_str(self, Allocator allocator) @nodiscard
{
	return (String)self.copy_zstr(allocator)[:self.len()];
}

<*
 Copy the contents into a newly allocated String, using the temp allocator.

 @return "A newly allocated copy of the contents"
*>
fn String DString.tcopy_str(self) @nodiscard => self.copy_str(tmem) @inline;

<*
 Compare the contents of two DStrings for equality. Two uninitialized (or empty) DStrings
 are considered equal.

 @param [in] other_string : "The DString to compare against"
 @return "true if both have the same contents"
*>
fn bool DString.equals(self, DString other_string)
{
	StringData *str1 = self.data();
	StringData *str2 = other_string.data();
	if (str1 == str2) return true;
	if (!str1) return str2.len == 0;
	if (!str2) return str1.len == 0;
	sz str1_len = str1.len;
	if (str1_len != str2.len) return false;
	for (sz i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] != str2.chars[i]) return false;
	}
	return true;
}

<*
 Release the DString's backing memory and reset it to uninitialized. A no-op on an
 already-uninitialized DString.

 @param [&drop] self
*>
fn void DString.free(&self)
{
	if (!*self) return;
	StringData* data = self.data();
	if (!data) return;
	alloc::free(data.allocator, data);
	*self = (DString)null;
}

<*
 Lexicographically compare two DStrings byte-by-byte, treating an uninitialized DString as
 empty and always less than a non-empty one.

 @param [in] other_string : "The DString to compare against"
 @return "true if self sorts before other_string"
*>
fn bool DString.less(self, DString other_string)
{
	StringData* str1 = self.data();
	StringData* str2 = other_string.data();
	if (str1 == str2 || !str2) return false;
	if (!str1) return str2.len > 0;
	sz str1_len = str1.len;
	sz str2_len = str2.len;
	sz len = math::min(str1_len, str2_len);
	foreach (i, s : str1.chars[:len])
	{
		char c = str2.chars[i];
		if (c > s) return true;
		if (c < s) return false;
	}
	return str1_len < str2_len;
}

<*
 Append raw bytes. Lazily initializes an uninitialized DString using the temp allocator.

 @param [in] bytes : "The bytes to append"
*>
fn void DString.append_bytes(&self, char[] bytes)
{
	sz other_len = bytes.len;
	if (!other_len) return;
	StringData* data = self.data();
	if (@unlikely(!data))
	{
		*self = temp_with_capacity(MIN_CAPACITY > other_len ? MIN_CAPACITY : other_len);
		data = self.data();
	}
	else if (@unlikely(data.len + other_len > data.capacity))
	{
		_reserve_inline(self, other_len);
		data = self.data();
	}
	mem::copy(&data.chars[data.len], bytes.ptr, other_len);
	data.len += other_len;
}

<*
 Decode the contents as UTF8 and return them as a newly allocated array of UTF32 codepoints.

 @param [&inout] allocator : "The allocator to use"
 @return "A newly allocated array of codepoints"
*>
fn Char32[] DString.copy_utf32(&self, Allocator allocator)
{
	return self.str_view().to_utf32(allocator) @inline!!;
}

<*
 Append a String.

 @param [in] str : "The string to append"
*>
fn void DString.append_string(&self, String str)
{
	self.append_bytes((String)str) @inline;
}

<*
 Append the contents of another DString. A no-op if `str` is uninitialized.

 @param [in] str : "The DString to append"
*>
fn void DString.append_dstring(&self, DString str)
{
	StringData* other = str.data();
	if (!other) return;
	self.append_bytes(str.str_view()) @inline;
}

<*
 Reset the DString's length to 0 without releasing its capacity. A no-op on an uninitialized
 DString.
*>
fn void DString.clear(self)
{
	if (!self) return;
	self.data().len = 0;
}

<*
 `OutStream` implementation: append `buffer`.

 @param [in] buffer : "The bytes to write"
 @return "The number of bytes written"
*>
fn sz? DString.write(&self, char[] buffer) @dynamic
{
	self.append_bytes(buffer) @inline;
	return buffer.len;
}

<*
 `OutStream` implementation: append a single byte.

 @param c : "The byte to write"
*>
fn void? DString.write_byte(&self, char c) @dynamic
{
	self.append_char(c) @inline;
}

<*
 Append a single character.

 @param c : "The character to append"
*>
fn void DString.append_char(&self, char c)
{
	self.reserve(1);
	StringData* data = self.data();
	data.chars[data.len++] = c;
}

<*
 Delete the inclusive range `[start, end]`.

 @param start : "The first index to delete"
 @param end : "The last index to delete (inclusive)"

 @require [start] (usz)start < (usz)self.len() : "start must be between 0 and self.len()"
 @require [end] (usz)end < (usz)self.len() : "end must be between 0 and self.len()"
 @require [end] end >= start : "End must be same or equal to the start"
*>
fn void DString.delete_range(&self, sz start, sz end)
{
	self.delete(start, end - start + 1);
}

<*
 Delete `len` characters starting at `start`.

 @param start : "The first index to delete"
 @param len : "The number of characters to delete, defaults to 1"

 @require [start] (usz)start < (usz)self.len() : "Start must be between 0 and string length"
 @require [len] len >= 0 : "The length must not be negative."
 @require [len] start + len <= self.len()
*>
fn void DString.delete(&self, sz start, sz len = 1)
{
	if (!len) return;
	StringData* data = self.data();
	sz new_len = data.len - len;
	if (new_len == 0)
	{
		data.len = 0;
		return;
	}
	sz len_after = data.len - start - len;
	if (len_after > 0)
	{
		data.chars[start:len_after] = data.chars[start + len:len_after];
	}
	data.len = new_len;
}

<*
 Append a value of any appendable type: `char`, `ichar`, `DString`, `String`, `Char32`, or any
 type convertible to `Char32` or `String`.

 @param value : "The value to append"
*>
macro void DString.append(&self, value)
{
	var $Type = $Typeof(value);
	$switch $Type:
		$case char:
		$case ichar:
			self.append_char(value);
		$case DString:
			self.append_dstring(value);
		$case String:
			self.append_string(value);
		$case Char32:
			self.append_char32(value);
		$default:
			$switch:
				$case $defined((Char32)value):
					self.append_char32((Char32)value);
				$case $defined((String)value):
					self.append_string((String)value);
				$default:
					$error "Unsupported type for append – use appendf instead.";
			$endswitch
	$endswitch
}

<*
 Same as DString.append, but may be faster due to inlining.

 @param value : "The value to append"
*>
macro void DString.append_inline(&self, value)
{
	var $Type = $Typeof(value);
	$switch $Type:
		$case char:
		$case ichar:
			self.append_char(value) @inline;
		$case DString:
			self.append_dstring(value) @inline;
		$case String:
			self.append_string(value) @inline;
		$case Char32:
			self.append_char32(value) @inline;
		$default:
			$switch:
				$case $defined((Char32)value):
					self.append_char32((Char32)value) @inline;
				$case $defined((String)value):
					self.append_string((String)value) @inline;
				$default:
					$error "Unsupported type for append_inline – use appendf instead.";
			$endswitch
	$endswitch
}

<*
 Insert a string at `index`, shifting any existing content after it forward. `s` may safely
 alias (be a view into) `self`'s own contents.

 @param index : "The index to insert at"
 @param s : "The string to insert"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
*>
fn void DString.insert_chars_at(&self, sz index, String s)
{
	if (s.len == 0) return;
	self.reserve(s.len);
	StringData* data = self.data();
	sz len = self.len();
	if (data.chars[:len].ptr == s.ptr)
	{
		// Source and destination are the same: nothing to do.
		return;
	}
	index = min(index, len);
	data.len += s.len;

	char* start = data.chars[index:s.len].ptr; // area to insert into
	mem::move(start + s.len, start, len - index); // move existing data
	switch
	{
		case s.ptr <= start && start < s.ptr + s.len:
			// Overlapping areas.
			foreach_r (i, c : s)
			{
				data.chars[index + i] = c;
			}
		case start <= s.ptr && s.ptr < start + len:
			// Source has moved.
			mem::move(start, s.ptr + s.len, s.len);
		default:
			mem::move(start, s, s.len);
	}
}

<*
 Insert another DString's contents at `index`. A no-op if `str` is uninitialized.

 @param index : "The index to insert at"
 @param str : "The DString to insert"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
*>
fn void DString.insert_string_at(&self, sz index, DString str)
{
	StringData* other = str.data();
	if (!other) return;
	self.insert_at(index, str.str_view());
}

<*
 Insert a single character at `index`.

 @param index : "The index to insert at"
 @param c : "The character to insert"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
*>
fn void DString.insert_char_at(&self, sz index, char c)
{
	self.reserve(1);
	StringData* data = self.data();

	char* start = &data.chars[index];
	mem::move(start + 1, start, self.len() - index);
	data.chars[index] = c;
	data.len++;
}

<*
 Insert a single UTF32 codepoint at `index`, encoded as UTF8.

 @param index : "The index to insert at"
 @param c : "The codepoint to insert"
 @return "The number of bytes inserted (1-4)"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
 @require [c] c <= 0x10ffff : "The character must be a valid codepoint"
*>
fn sz DString.insert_char32_at(&self, sz index, Char32 c)
{
	char[4] buffer @noinit;
	char* p = &buffer;
	sz n = conv::char32_to_utf8_unsafe(c, &p);

	self.reserve(n);
	StringData* data = self.data();

	char* start = &data.chars[index];
	mem::move(start + n, start, self.len() - index);
	data.chars[index:n] = buffer[:n];
	data.len += n;

	return n;
}

<*
 Insert a sequence of UTF32 codepoints at `index`, encoding each as UTF8.

 @param index : "The index to insert at"
 @param [in] chars : "The codepoints to insert"
 @return "The number of bytes inserted"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
*>
fn sz DString.insert_utf32_at(&self, sz index, Char32[] chars)
{
	sz n = conv::utf8len_for_utf32(chars);

	self.reserve(n);
	StringData* data = self.data();

	char* start = &data.chars[index];
	mem::move(start + n, start, self.len() - index);

	char[4] buffer @noinit;

	foreach(c : chars)
	{
		char* p = &buffer;
		sz m = conv::char32_to_utf8_unsafe(c, &p);
		data.chars[index:m] = buffer[:m];
		index += m;
	}

	data.len += n;

	return n;
}

<*
 Insert a value of any insertable type at `index`: `char`, `ichar`, `DString`, `String`,
 `Char32`, or any type convertible to `Char32` or `String`.

 @param index : "The index to insert at"
 @param value : "The value to insert"

 @require [index] (usz)index <= (usz)self.len() : "Insert must be between 0 and self.length()"
*>
macro void DString.insert_at(&self, sz index, value)
{
	var $Type = $Typeof(value);
	$switch $Type:
		$case char:
		$case ichar:
			self.insert_char_at(index, value);
		$case DString:
			self.insert_string_at(index, value);
		$case String:
			self.insert_chars_at(index, value);
		$case Char32:
			self.insert_char32_at(index, value);
		$default:
			$switch:
				$case $defined((Char32)value):
					self.insert_char32_at(index, (Char32)value);
				$case $defined((String)value):
					self.insert_chars_at(index, (String)value);
				$default:
					$error "Unsupported type for insert";
			$endswitch
	$endswitch
}

<*
 Append the result of formatting `format` with `args`, printf-style. Lazily initializes an
 uninitialized DString using the temp allocator.

 @param [in] format : "The format string"
 @param args : "The arguments referenced by the format string"
 @return "The number of bytes appended"
*>
fn sz? DString.appendf(&self, String format, args...) @maydiscard @format(1)
{
	if (!self.data()) self.tinit(format.len + 20);
	Formatter formatter;
	formatter.init(&append_fn, self);
	return formatter.vprintf(format, args);
}

<*
 Same as `appendf`, but also appends a trailing newline.

 @param format : "The format string"
 @param args : "The arguments referenced by the format string"
 @return "The number of bytes appended, including the trailing newline"
*>
fn sz? DString.appendfn(&self, String format, args...) @maydiscard @format(1)
{
	if (!self.data()) self.tinit(format.len + 20);
	@pool()
	{
		Formatter formatter;
		formatter.init(&append_fn, self);
		sz len = formatter.vprintf(format, args)!;
		self.append('\n');
		return len + 1;
	};
}

<*
 Join an array of strings into a new DString, separated by `joiner`.

 @param [&inout] allocator : "The allocator to use"
 @param [in] s : "The strings to join"
 @param [in] joiner : "The separator inserted between each pair of strings"
 @return "A newly allocated DString containing the joined strings"
*>
fn DString join(Allocator allocator, String[] s, String joiner) @nodiscard
{
	if (!s.len) return new(allocator);
	sz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	DString res = new_with_capacity(allocator, total_size);
	res.append(s[0]);
	foreach (String str : s[1..])
	{
		res.append(joiner);
		res.append(str);
	}
	return res;
}

fn void? append_fn(void* data, char c)
{
	DString* s = data;
	s.append_char(c) @inline;
}

<*
 Reverse the DString's contents in place. A no-op on an uninitialized DString.
*>
fn void DString.reverse(self)
{
	StringData *data = self.data();
	if (!data) return;
	sz mid = data.len / 2;
	for (sz i = 0; i < mid; i++)
	{
		char temp = data.chars[i];
		sz reverse_index = data.len - 1 - i;
		data.chars[i] = data.chars[reverse_index];
		data.chars[reverse_index] = temp;
	}
}

macro StringData* DString.data(self)
{
	return (StringData*)self;
}

<*
 Ensure at least `addition` more bytes of capacity are available beyond the current length.
 Lazily initializes an uninitialized DString using the temp allocator.

 @param addition : "The number of additional bytes to make room for"
 @require [addition] addition >= 0 : "Negative reserve is not allowed"
*>
fn void DString.reserve(&self, sz addition) @inline
{
	if (!addition) return;
	if (!*self)
	{
		*self = dstring::temp_with_capacity(addition);
		return;
	}
	_reserve_inline(self, addition);
}

<*
 Read the entire contents of `reader` and append them.

 @param [&inout] reader : "The stream to read from"
 @return "The total number of bytes read and appended"
*>
fn sz? DString.read_from_stream(&self, InStream reader)
{
	if (&reader.available)
	{
		sz total_read = 0;
		while (long available = reader.available()!)
		{
			if (available > sz::max) available = (ulong)sz::max;
			self.reserve((sz)available);
			StringData* data = self.data();
			sz len = io::read_buffer(reader, data.chars[data.len..(data.capacity - 1)])!;
			if (!len) break;
			total_read += len;
			data.len += len;
		}
		return total_read;
	}
	sz total_read = 0;
	while (true)
	{
		// Reserve at least 16 bytes
		self.reserve(16);
		StringData* data = self.data();
		// Read into the rest of the buffer
		sz read = io::read_buffer(reader, data.chars[data.len..(data.capacity - 1)])!;
		data.len += read;
		total_read += read;
		// Ok, we reached the end.
		if (read < 16) return total_read;
		// Otherwise go another round
	}
}

<*
 The backing allocation for a DString: a small header immediately followed by the character
 data (`chars` is a flexible array member spanning the rest of the allocation).
*>
struct StringData @private
{
	Allocator allocator;
	sz len;
	sz capacity;
	char[*] chars;
}

<*
 Grow `self` if necessary so that at least `addition` more bytes fit beyond the current length.

 @param [&inout] self : "The DString to grow"
 @param addition : "The number of additional bytes to make room for"
 @require self.data() != null
 @require [addition] (usz)addition < (usz)(sz::max - self.len())
*>
macro _reserve_inline(DString *self, sz addition)
{
	StringData* data = self.data();
	sz len = data.len + addition;
	if (@likely(data.capacity >= len)) return;
	*self = (DString)_reserve_slow(data, len);
}

<*
 Grow `data` to hold at least `new_len` bytes, reallocating its backing memory.

 @param [&drop] data : "The StringData to grow"
 @param new_len : "The total number of bytes that must fit"
 @return "The (possibly relocated) grown StringData"
 @require [new_len] new_len > data.capacity
*>
fn StringData* _reserve_slow(StringData* data, sz new_len) @local
{
	sz new_capacity = data.capacity * 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	if (new_capacity < new_len) new_capacity = math::next_power_of_2(new_len);
	data.capacity = new_capacity;
	return alloc::realloc(data.allocator, data, StringData::size + new_capacity);
}
