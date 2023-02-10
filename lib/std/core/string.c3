module std::core::string;
import libc;

define VarString = distinct void*;
define DynStr = VarString;
define DynString = VarString;
define DString = VarString;
define VString = VarString;

private struct StringData
{
	Allocator* allocator;
	usz len;
	usz capacity;
	char[*] chars;
}

const usz MIN_CAPACITY = 16;

fn VarString new_with_capacity(usz capacity, Allocator* allocator = mem::current_allocator())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = allocator.alloc(StringData.sizeof + capacity)!!;
	data.allocator = allocator;
	data.len = 0;
	data.capacity = capacity;
	return (VarString)data;
}

fn VarString new(String c)
{
	usz len = c.len;
	VarString str = new_with_capacity(len);
	StringData* data = str.data();
	if (len)
	{
		data.len = len;
		mem::copy(&data.chars, c.ptr, len);
	}
	return (VarString)data;
}

fn ZString VarString.zstr(VarString str)
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

fn usz VarString.capacity(VarString this)
{
	if (!this) return 0;
	return this.data().capacity;
}

fn usz VarString.len(VarString this)
{
	if (!this) return 0;
	return this.data().len;
}

/**
 * @require new_size <= this.len()
 */
fn void VarString.chop(VarString this, usz new_size)
{
	if (!this) return;
	this.data().len = new_size;
}

fn String VarString.str(VarString str)
{
	StringData* data = (StringData*)str;
	if (!data) return String {};
	return (String)data.chars[:data.len];
}

fn void VarString.append_utf32(VarString* str, Char32[] chars)
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
fn void VarString.set(VarString str, usz index, char c)
{
	str.data().chars[index] = c;
}

fn void VarString.append_repeat(VarString* str, char c, usz times)
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
fn void VarString.append_char32(VarString* str, Char32 c)
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

fn VarString VarString.tcopy(VarString* str) => str.copy(mem::temp_allocator());

fn VarString VarString.copy(VarString* str, Allocator* allocator = null)
{
	if (!str)
	{
		if (allocator) return new_with_capacity(0, allocator);
		return (VarString)null;
	}
	if (!allocator) allocator = mem::current_allocator();
	StringData* data = str.data();
	VarString new_string = new_with_capacity(data.capacity, allocator);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString VarString.copy_zstr(VarString* str, Allocator* allocator = mem::current_allocator())
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

fn String VarString.copy_str(VarString* str, Allocator* allocator = mem::current_allocator())
{
	return (String)str.copy_zstr(allocator)[:str.len()];
}

fn String VarString.tcopy_str(VarString* str) => str.copy_str(mem::temp_allocator()) @inline;

fn bool VarString.equals(VarString str, VarString other_string)
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

fn void VarString.destroy(VarString* str)
{
	if (!*str) return;
	StringData* data = str.data();
	if (!data) return;
	data.allocator.free(data)!!;
	*str = (VarString)null;
}

fn bool VarString.less(VarString str, VarString other_string)
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

fn void VarString.append_chars(VarString* this, String str)
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

fn Char32[] VarString.copy_utf32(VarString* this, Allocator* allocator = mem::current_allocator())
{
	return str::utf8to32(this.str(), allocator) @inline!!;
}

fn void VarString.append_string(VarString* this, VarString str)
{
	StringData* other = (StringData*)str;
	if (!other) return;
	this.append(str.str());
}

fn void VarString.clear(VarString* str)
{
	str.data().len = 0;
}

fn void VarString.append_char(VarString* str, char c)
{
	if (!*str)
	{
		*str = new_with_capacity(MIN_CAPACITY);
	}
	str.reserve(1);
	StringData* data = (StringData*)*str;
	data.chars[data.len++] = c;
}


macro void VarString.append(VarString* str, value)
{
	var $Type = $typeof(value);
	$switch ($Type):
		$case char:
		$case ichar:
			str.append_char(value);
		$case VarString:
			str.append_string(value);
		$case String:
			str.append_chars(value);
		$case Char32:
			str.append_char32(value);
		$default:
			$if (@convertible(value, Char32)):
				str.append_char32(value);
			$elif (@convertible(value, String)):
				str.append_chars(value);
			$else:
				$assert(false, "Unsupported type for append – use printf instead.");
			$endif;
	$endswitch;
}


private fn StringData* VarString.data(VarString str) @inline
{
	return (StringData*)str;
}

private fn void VarString.reserve(VarString* str, usz addition)
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
	*str = (VarString)data.allocator.realloc(data, StringData.sizeof + new_capacity)!!;
}

fn VarString VarString.new_concat(VarString a, VarString b, Allocator* allocator = mem::current_allocator())
{
	VarString string = new_with_capacity(a.len() + b.len(), allocator);
	string.append(a);
	string.append(b);
	return string; 
}
