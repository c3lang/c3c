module std::core::string;
import libc;

define String = distinct void*;

private struct StringData
{
	Allocator allocator;
	usize len;
	usize capacity;
	char[*] chars;
}

const usize MIN_CAPACITY = 16;

fn String new_with_capacity(usize capacity, Allocator allocator = { null, null })
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	if (!allocator.function)
	{
		allocator = mem::current_allocator();
	}
	assert(allocator.function, "Expected an allocator to be present.");
	StringData* data = allocator.alloc(StringData.sizeof + capacity)!!;
	data.allocator = allocator;
	data.len = 0;
	data.capacity = capacity;
	return (String)data;
}

fn String new(char[] c)
{
	usize len = c.len;
	String str = new_with_capacity(len);
	StringData* data = str.data();
	if (len)
	{
		data.len = len;
		mem::copy(&data.chars, c.ptr, len);
	}
	return (String)data;
}

fn ZString String.zstr(String str)
{
	StringData* data = str.data();
	if (!data) return (ZString)"";
	if (data.capacity == data.len)
	{
		str.reserve(1);
		data.chars[data.len] = 0;
	}
	else if (data.chars[data.len] != 0)
	{
		data.chars[data.len] = 0;
	}
	return (ZString)&data.chars[0];
}

fn usize String.len(String this)
{
	if (!this) return 0;
	return this.data().len;
}

/**
 * @require new_size <= this.len()
 */
fn void String.chop(String this, usize new_size)
{
	if (!this) return;
	this.data().len = new_size;
}

fn char[] String.str(String str)
{
	StringData* data = (StringData*)str;
	return data.chars[0..data.len - 1];
}

fn void String.append_utf32(String* str, Char32[] chars)
{
	str.reserve(chars.len);
	foreach (Char32 c : chars)
	{
		str.append_char32(c);
	}
}

/**
 * @require index < str.len()
 **/
fn void String.set(String str, usize index, char c)
{
	str.data().chars[index] = c;
}

fn void String.append_repeat(String* str, char c, usize times)
{
	if (times == 0) return;
	str.reserve(times);
	StringData* data = str.data();
	for (usize i = 0; i < times; i++)
	{
        data.chars[data.len++] = c;
	}
}

/**
 * @require c < 0x10ffff
 */
fn void String.append_char32(String* str, Char32 c)
{
	if (c < 0x7f)
	{
		str.reserve(1);
		StringData* data = str.data();
        data.chars[data.len++] = (char)c;
        return;
    }
    if (c < 0x7ff)
    {
		str.reserve(2);
		StringData* data = str.data();
		data.chars[data.len++] = (char)(0xC0 | c >> 6);
        data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
        return;
    }
    if (c < 0xffff)
    {
		str.reserve(3);
		StringData* data = str.data();
		data.chars[data.len++] = (char)(0xE0 | c >> 12);
        data.chars[data.len++] = (char)(0x80 | (c >> 6 & 0x3F));
        data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
        return;
    }
	str.reserve(4);
	StringData* data = str.data();
	data.chars[data.len++] = (char)(0xF0 | c >> 18);
    data.chars[data.len++] = (char)(0x80 | (c >> 12 & 0x3F));
    data.chars[data.len++] = (char)(0x80 | (c >> 6 & 0x3F));
    data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
}

fn String String.copy(String* str, Allocator allocator = { null, null })
{
	if (!str)
	{
		if (allocator.function) return new_with_capacity(0, allocator);
		return (String)null;
	}
	if (!allocator.function) allocator = mem::current_allocator();
	StringData* data = str.data();
	String new_string = new_with_capacity(data.capacity, allocator);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString String.copy_zstr(String* str, Allocator allocator = { null, null })
{
	usize str_len = str.len();
	if (!str_len)
	{
		if (allocator.function) return (ZString)allocator.calloc(1, 1)!!;
		return (ZString)mem::calloc(1, 1);
	}
	if (!allocator.function) allocator = mem::current_allocator();
	char* zstr = allocator.alloc(str_len + 1)!!;
	StringData* data = str.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn bool String.equals(String str, String other_string)
{
	StringData *str1 = str.data();
	StringData *str2 = other_string.data();
	if (str1 == str2) return true;
	if (!str1) return str2.len == 0;
	if (!str2) return str1.len == 0;
	usize str1_len = str1.len;
	if (str1_len != str2.len) return false;
	for (int i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] != str2.chars[i]) return false;
	}
	return true;
}

fn void String.destroy(String* str)
{
	if (!*str) return;
	StringData* data = str.data();
	if (!data) return;
	data.allocator.free(data)!!;
	*str = (String)null;
}

fn bool String.less(String str, String other_string)
{
	StringData* str1 = str.data();
	StringData* str2 = other_string.data();
	if (str1 == str2) return false;
	if (!str1) return str2.len != 0;
	if (!str2) return str1.len == 0;
	usize str1_len = str1.len;
	usize str2_len = str2.len;
	if (str1_len != str2_len) return str1_len < str2_len;
	for (int i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] >= str2.chars[i]) return false;
	}
	return true;
}

fn void String.append_chars(String* this, char[] str)
{
	usize other_len = str.len;
	if (!other_len) return;
	if (!*this)
	{
		*this = new(str);
		return;
	}
	this.reserve(other_len);
	StringData* data = (StringData*)*this;
	mem::copy(&data.chars[data.len], str.ptr, other_len);
	data.len += other_len;
}

fn Char32[] String.copy_utf32(String* this, Allocator allocator = { null, null })
{
	return str::utf8to32(this.str(), allocator) @inline!!;
}

fn void String.append_string(String* this, String str)
{
	StringData* other = (StringData*)str;
	if (!other) return;
	this.append(str.str());
}

fn void String.append_char(String* str, char c)
{
	if (!*str)
	{
		*str = new_with_capacity(MIN_CAPACITY);
	}
	str.reserve(1);
	StringData* data = (StringData*)*str;
	data.chars[data.len++] = c;
}


macro void String.append(String* str, value)
{
	var $Type = $typeof(value);
	$switch ($Type):
		$case char:
		$case ichar:
			str.append_char(value);
		$case String:
			str.append_string(value);
		$case char[]:
			str.append_chars(value);
		$case Char32:
			str.append_char32(value);
		$default:
			$if ($convertible($Type, Char32)):
				str.append_char32(value);
			$elif ($convertible($Type, char[])):
				str.append_chars(value);
			$else:
				$assert("Unsupported type for appending");
			$endif;
	$endswitch;
}


private fn StringData* String.data(String str) @inline
{
	return (StringData*)str;
}

private fn void String.reserve(String* str, usize addition)
{
	StringData* data = str.data();
	if (!data)
	{
		*str = string::new_with_capacity(addition);
		return;
	}
	usize len = data.len + addition;
	if (data.capacity >= len) return;
	usize new_capacity = data.capacity * 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	*str = (String)data.allocator.realloc(data, StringData.sizeof + new_capacity)!!;
}
