module std::core::string;
import libc;

define String = distinct void*;

private struct StringData
{
	Allocator* allocator;
	usz len;
	usz capacity;
	char[*] chars;
}

const usz MIN_CAPACITY = 16;

fn String new_with_capacity(usz capacity, Allocator* allocator = mem::current_allocator())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = allocator.alloc(StringData.sizeof + capacity)!!;
	data.allocator = allocator;
	data.len = 0;
	data.capacity = capacity;
	return (String)data;
}

fn String new(char[] c)
{
	usz len = c.len;
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

fn usz String.len(String this)
{
	if (!this) return 0;
	return this.data().len;
}

/**
 * @require new_size <= this.len()
 */
fn void String.chop(String this, usz new_size)
{
	if (!this) return;
	this.data().len = new_size;
}

fn char[] String.str(String str)
{
	StringData* data = (StringData*)str;
	if (!data) return char[] {};
	return data.chars[:data.len];
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
fn void String.set(String str, usz index, char c)
{
	str.data().chars[index] = c;
}

fn void String.append_repeat(String* str, char c, usz times)
{
	if (times == 0) return;
	str.reserve(times);
	StringData* data = str.data();
	for (usz i = 0; i < times; i++)
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

fn String String.tcopy(String* str) = str.copy(mem::temp_allocator());

fn String String.copy(String* str, Allocator* allocator = null)
{
	if (!str)
	{
		if (allocator) return new_with_capacity(0, allocator);
		return (String)null;
	}
	if (!allocator) allocator = mem::current_allocator();
	StringData* data = str.data();
	String new_string = new_with_capacity(data.capacity, allocator);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString String.copy_zstr(String* str, Allocator* allocator = mem::current_allocator())
{
	usz str_len = str.len();
	if (!str_len)
	{
		return (ZString)allocator.calloc(1)!!;
	}
	char* zstr = allocator.alloc(str_len + 1)!!;
	StringData* data = str.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn char[] String.copy_str(String* str, Allocator* allocator = mem::current_allocator())
{
	return str.copy_zstr(allocator)[:str.len()];
}

fn char[] String.tcopy_str(String* str) = str.copy_str(mem::temp_allocator()) @inline;

fn bool String.equals(String str, String other_string)
{
	StringData *str1 = str.data();
	StringData *str2 = other_string.data();
	if (str1 == str2) return true;
	if (!str1) return str2.len == 0;
	if (!str2) return str1.len == 0;
	usz str1_len = str1.len;
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
	usz str1_len = str1.len;
	usz str2_len = str2.len;
	if (str1_len != str2_len) return str1_len < str2_len;
	for (int i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] >= str2.chars[i]) return false;
	}
	return true;
}

fn void String.append_chars(String* this, char[] str)
{
	usz other_len = str.len;
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

fn Char32[] String.copy_utf32(String* this, Allocator* allocator = mem::current_allocator())
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
			$if (@convertible($Type, Char32)):
				str.append_char32(value);
			$elif (@convertible($Type, char[])):
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

private fn void String.reserve(String* str, usz addition)
{
	StringData* data = str.data();
	if (!data)
	{
		*str = string::new_with_capacity(addition);
		return;
	}
	usz len = data.len + addition;
	if (data.capacity >= len) return;
	usz new_capacity = data.capacity *= 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	*str = (String)data.allocator.realloc(data, StringData.sizeof + new_capacity)!!;
}

fn String String.new_concat(String a, String b, Allocator* allocator = mem::current_allocator())
{
	String resultString = new_with_capacity(a.len() + b.len(), allocator);
	resultString.append(a);
	resultString.append(b);
	return resultString; 
}
