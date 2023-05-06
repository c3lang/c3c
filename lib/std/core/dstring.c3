module std::core::dstring;

typedef DString = distinct void*;

const usz MIN_CAPACITY @private = 16;

/**
 * @require !str.data() "String already initialized"
 **/
fn void DString.init(DString *str, usz capacity = MIN_CAPACITY, Allocator* using = mem::heap())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = malloc(StringData, 1, .using = using, .end_padding = capacity);
	data.allocator = using;
	data.len = 0;
	data.capacity = capacity;
	*str = (DString)data;
}

/**
 * @require !str.data() "String already initialized"
 **/
fn void DString.tinit(DString *str, usz capacity = MIN_CAPACITY) => str.init(capacity, mem::temp()) @inline;

fn DString new_with_capacity(usz capacity, Allocator* using = mem::heap())
{
	DString dstr;
	dstr.init(capacity, using);
	return dstr;
}

fn DString tnew_with_capacity(usz capacity) => new_with_capacity(capacity, mem::temp()) @inline;

fn DString new(String c = "", Allocator* using = mem::heap())
{
	usz len = c.len;
	StringData* data = (StringData*)new_with_capacity(len, using);
	if (len)
	{
		data.len = len;
		mem::copy(&data.chars, c.ptr, len);
	}
	return (DString)data;
}

fn DString tnew(String s = "") => new(s, mem::temp()) @inline;

fn DString DString.new_concat(DString a, DString b, Allocator* using = mem::heap())
{
	DString string;
	string.init(a.len() + b.len(), using);
	string.append(a);
	string.append(b);
	return string;
}

fn DString DString.new_tconcat(DString a, DString b) => a.new_concat(b, mem::temp());

fn ZString DString.zstr(DString str)
{
	StringData* data = str.data();
	if (!data) return "";
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

fn usz DString.capacity(DString this)
{
	if (!this) return 0;
	return this.data().capacity;
}

fn usz DString.len(DString this)
{
	if (!this) return 0;
	return this.data().len;
}

/**
 * @require new_size <= this.len()
 */
fn void DString.chop(DString this, usz new_size)
{
	if (!this) return;
	this.data().len = new_size;
}

fn String DString.str(DString str)
{
	StringData* data = (StringData*)str;
	if (!data) return "";
	return (String)data.chars[:data.len];
}

fn void DString.append_utf32(DString* str, Char32[] chars)
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
fn void DString.set(DString str, usz index, char c)
{
	str.data().chars[index] = c;
}

fn void DString.append_repeat(DString* str, char c, usz times)
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
 * @require c <= 0x10ffff
 */
fn void DString.append_char32(DString* str, Char32 c)
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

fn DString DString.tcopy(DString* str) => str.copy(mem::temp());

fn DString DString.copy(DString* str, Allocator* using = null)
{
	if (!str)
	{
		if (using) return new_with_capacity(0, using);
		return (DString)null;
	}
	if (!using) using = mem::heap();
	StringData* data = str.data();
	DString new_string = new_with_capacity(data.capacity, using);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString DString.copy_zstr(DString* str, Allocator* using = mem::heap())
{
	usz str_len = str.len();
	if (!str_len)
	{
		return (ZString)calloc(1, .using = using);
	}
	char* zstr = malloc(str_len + 1, .using = using);
	StringData* data = str.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn String DString.copy_str(DString* str, Allocator* using = mem::heap())
{
	return (String)str.copy_zstr(using)[:str.len()];
}

fn String DString.tcopy_str(DString* str) => str.copy_str(mem::temp()) @inline;

fn bool DString.equals(DString str, DString other_string)
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

fn void DString.free(DString* str)
{
	if (!*str) return;
	StringData* data = str.data();
	if (!data) return;
	free(data, .using = data.allocator);
	*str = (DString)null;
}

fn bool DString.less(DString str, DString other_string)
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

fn void DString.append_chars(DString* this, String str)
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

fn Char32[] DString.copy_utf32(DString* this, Allocator* using = mem::heap())
{
	return this.str().to_utf32(using) @inline!!;
}

fn void DString.append_string(DString* this, DString str)
{
	StringData* other = (StringData*)str;
	if (!other) return;
	this.append(str.str());
}

fn void DString.clear(DString* str)
{
	str.data().len = 0;
}

fn void DString.append_char(DString* str, char c)
{
	if (!*str)
	{
		*str = new_with_capacity(MIN_CAPACITY);
	}
	str.reserve(1);
	StringData* data = (StringData*)*str;
	data.chars[data.len++] = c;
}


macro void DString.append(DString* str, value)
{
	var $Type = $typeof(value);
	$switch ($Type)
		$case char:
		$case ichar:
			str.append_char(value);
		$case DString:
			str.append_string(value);
		$case String:
			str.append_chars(value);
		$case Char32:
			str.append_char32(value);
		$default:
			$switch
				$case @convertible(value, Char32):
					str.append_char32(value);
				$case @convertible(value, String):
					str.append_chars(value);
				$default:
					$error "Unsupported type for append – use printf instead.";
			$endswitch
	$endswitch
}


fn usz! DString.printf(DString* str, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, str);
	return formatter.vprintf(format, args);
}

fn usz! DString.printfn(DString* str, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, str);
	usz len = formatter.vprintf(format, args)!;
	str.append('\n');
	return len + 1;
}

fn DString new_join(String[] s, String joiner, Allocator* using = mem::heap())
{
	if (!s.len) return (DString)null;
	usz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	DString res = new_with_capacity(total_size, using);
	res.append(s[0]);
	foreach (String* &str : s[1..])
	{
		res.append(joiner);
		res.append(*str);
	}
	return res;
}

fn void! out_string_append_fn(char c, void* data) @private
{
	DString* s = data;
	s.append_char(c);
}


fn StringData* DString.data(DString str) @inline @private
{
	return (StringData*)str;
}

fn void DString.reserve(DString* str, usz addition)
{
	StringData* data = str.data();
	if (!data)
	{
		*str = dstring::new_with_capacity(addition);
		return;
	}
	usz len = data.len + addition;
	if (data.capacity >= len) return;
	usz new_capacity = data.capacity *= 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	*str = (DString)realloc(data, StringData.sizeof + new_capacity, .using = data.allocator);
}

fn usz! DString.read_from_stream(DString* string, Stream* reader)
{
	if (reader.supports_available())
	{
		usz total_read = 0;
		while (usz available = reader.available()!)
		{
			string.reserve(available);
			StringData* data = string.data();
			usz len = reader.read(data.chars[data.len..(data.capacity - 1)])!;
			total_read += len;
			data.len += len;
		}
		return total_read;
	}
	usz total_read = 0;
	while (true)
	{
		// Reserve at least 16 bytes
		string.reserve(16);
		StringData* data = string.data();
		// Read into the rest of the buffer
		usz read = reader.read(data.chars[data.len..(data.capacity - 1)])!;
		data.len += read;
		// Ok, we reached the end.
		if (read < 16) return total_read;
		// Otherwise go another round
	}
}

struct StringData @private
{
	Allocator* allocator;
	usz len;
	usz capacity;
	char[*] chars;
}
