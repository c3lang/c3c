module std::core::dstring;

def DString = distinct void*;

const usz MIN_CAPACITY @private = 16;

/**
 * @require !self.data() "String already initialized"
 **/
fn void DString.init(&self, usz capacity = MIN_CAPACITY, Allocator* using = mem::heap())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = malloc(StringData, 1, .using = using, .end_padding = capacity);
	data.allocator = using;
	data.len = 0;
	data.capacity = capacity;
	*self = (DString)data;
}

/**
 * @require !self.data() "String already initialized"
 **/
fn void DString.tinit(&self, usz capacity = MIN_CAPACITY) => self.init(capacity, mem::temp()) @inline;

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

fn DString DString.new_concat(self, DString b, Allocator* using = mem::heap())
{
	DString string;
	string.init(self.len() + b.len(), using);
	string.append(self);
	string.append(b);
	return string;
}

fn DString DString.new_tconcat(self, DString b) => self.new_concat(b, mem::temp());

fn ZString DString.zstr(self)
{
	StringData* data = self.data();
	if (!data) return "";
	if (data.capacity == data.len)
	{
		self.reserve(1);
		data.chars[data.len] = 0;
	}
	else if (data.chars[data.len] != 0)
	{
		data.chars[data.len] = 0;
	}
	return (ZString)&data.chars[0];
}

fn usz DString.capacity(self)
{
	if (!self) return 0;
	return self.data().capacity;
}

fn usz DString.len(self)
{
	if (!self) return 0;
	return self.data().len;
}

/**
 * @require new_size <= self.len()
 */
fn void DString.chop(self, usz new_size)
{
	if (!self) return;
	self.data().len = new_size;
}

fn String DString.str_view(self)
{
	StringData* data = (StringData*)self;
	if (!data) return "";
	return (String)data.chars[:data.len];
}

fn void DString.append_utf32(&self, Char32[] chars)
{
	self.reserve(chars.len);
	foreach (Char32 c : chars)
	{
		self.append_char32(c);
	}
}

/**
 * @require index < self.len()
 **/
fn void DString.set(self, usz index, char c)
{
	self.data().chars[index] = c;
}

fn void DString.append_repeat(&self, char c, usz times)
{
	if (times == 0) return;
	self.reserve(times);
	StringData* data = self.data();
	for (usz i = 0; i < times; i++)
	{
		data.chars[data.len++] = c;
	}
}

/**
 * @require c <= 0x10ffff
 */
fn void DString.append_char32(&self, Char32 c)
{
	if (c < 0x7f)
	{
		self.reserve(1);
		StringData* data = self.data();
		data.chars[data.len++] = (char)c;
		return;
	}
	if (c < 0x7ff)
	{
		self.reserve(2);
		StringData* data = self.data();
		data.chars[data.len++] = (char)(0xC0 | c >> 6);
		data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
		return;
	}
	if (c < 0xffff)
	{
		self.reserve(3);
		StringData* data = self.data();
		data.chars[data.len++] = (char)(0xE0 | c >> 12);
		data.chars[data.len++] = (char)(0x80 | (c >> 6 & 0x3F));
		data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
		return;
	}
	self.reserve(4);
	StringData* data = self.data();
	data.chars[data.len++] = (char)(0xF0 | c >> 18);
	data.chars[data.len++] = (char)(0x80 | (c >> 12 & 0x3F));
	data.chars[data.len++] = (char)(0x80 | (c >> 6 & 0x3F));
	data.chars[data.len++] = (char)(0x80 | (c & 0x3F));
}

fn DString DString.tcopy(&self) => self.copy(mem::temp());

fn DString DString.copy(self, Allocator* using = null)
{
	if (!self)
	{
		if (using) return new_with_capacity(0, using);
		return (DString)null;
	}
	if (!using) using = mem::heap();
	StringData* data = self.data();
	DString new_string = new_with_capacity(data.capacity, using);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString DString.copy_zstr(self, Allocator* using = mem::heap())
{
	usz str_len = self.len();
	if (!str_len)
	{
		return (ZString)calloc(1, .using = using);
	}
	char* zstr = malloc(str_len + 1, .using = using);
	StringData* data = self.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn String DString.copy_str(self, Allocator* using = mem::heap())
{
	return (String)self.copy_zstr(using)[:self.len()];
}

fn String DString.tcopy_str(self) => self.copy_str(mem::temp()) @inline;

fn bool DString.equals(self, DString other_string)
{
	StringData *str1 = self.data();
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

fn void DString.free(&self)
{
	if (!*self) return;
	StringData* data = self.data();
	if (!data) return;
	free(data, .using = data.allocator);
	*self = (DString)null;
}

fn bool DString.less(self, DString other_string)
{
	StringData* str1 = self.data();
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

fn void DString.append_chars(&self, String str)
{
	usz other_len = str.len;
	if (!other_len) return;
	if (!*self)
	{
		*self = new(str);
		return;
	}
	self.reserve(other_len);
	StringData* data = self.data();
	mem::copy(&data.chars[data.len], str.ptr, other_len);
	data.len += other_len;
}

fn Char32[] DString.copy_utf32(&self, Allocator* using = mem::heap())
{
	return self.str_view().to_utf32(using) @inline!!;
}

fn void DString.append_string(&self, DString str)
{
	StringData* other = (StringData*)str;
	if (!other) return;
	self.append(str.str_view());
}

fn void DString.clear(self)
{
	if (!self) return;
	self.data().len = 0;
}

fn void DString.append_char(&self, char c)
{
	if (!*self)
	{
		*self = new_with_capacity(MIN_CAPACITY);
	}
	self.reserve(1);
	StringData* data = (StringData*)*self;
	data.chars[data.len++] = c;
}


macro void DString.append(&self, value)
{
	var $Type = $typeof(value);
	$switch ($Type)
		$case char:
		$case ichar:
			self.append_char(value);
		$case DString:
			self.append_string(value);
		$case String:
			self.append_chars(value);
		$case Char32:
			self.append_char32(value);
		$default:
			$switch
				$case @convertible(value, Char32):
					self.append_char32(value);
				$case @convertible(value, String):
					self.append_chars(value);
				$default:
					$error "Unsupported type for append – use printf instead.";
			$endswitch
	$endswitch
}


fn usz! DString.printf(&self, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, self);
	return formatter.vprintf(format, args);
}

fn usz! DString.printfn(&self, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn, self);
	usz len = formatter.vprintf(format, args)!;
	self.append('\n');
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

fn void! out_string_append_fn(void* data, char c) @private
{
	DString* s = data;
	s.append_char(c);
}


fn StringData* DString.data(self) @inline @private
{
	return (StringData*)self;
}

fn void DString.reserve(&self, usz addition)
{
	StringData* data = self.data();
	if (!data)
	{
		*self = dstring::new_with_capacity(addition);
		return;
	}
	usz len = data.len + addition;
	if (data.capacity >= len) return;
	usz new_capacity = data.capacity * 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	while (new_capacity < len) new_capacity *= 2;
	data.capacity = new_capacity;
	*self = (DString)realloc(data, StringData.sizeof + new_capacity, .using = data.allocator);
}

fn usz! DString.read_from_stream(&self, Stream* reader)
{
	if (reader.supports_available())
	{
		usz total_read = 0;
		while (usz available = reader.available()!)
		{
			self.reserve(available);
			StringData* data = self.data();
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
		self.reserve(16);
		StringData* data = self.data();
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
