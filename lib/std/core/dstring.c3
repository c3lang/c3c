module std::core::dstring;

def DString = distinct void*;

struct StringData @private
{
	Allocator* allocator;
	usz len;
	usz capacity;
}

macro char* StringData.start(&data) => (char*)((void*)data - data.capacity);
macro char* StringData.end(&data) => (char*)((void*)data - data.capacity + data.len);

const usz MIN_CAPACITY @private = 16;

/**
 * @require !self.data() "String already initialized"
 **/
fn void DString.init(&self, usz capacity = MIN_CAPACITY, Allocator* using = mem::heap())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	char* start = malloc(char, capacity + StringData.sizeof, .using = using);
	*self = get_dstring(start, 0, capacity, using);
}

/**
 * @require !self.data() "String already initialized"
 **/
fn void DString.tinit(&self, usz capacity = MIN_CAPACITY) => self.init(capacity, mem::temp()) @inline;

fn void DString.free(&self)
{
	if (!*self) return;
	StringData* data = self.data();
	if (!data) return;
	free(data.start(), .using = data.allocator);
	*self = (DString)null;
}

fn DString new_with_capacity(usz capacity, Allocator* using = mem::heap())
{
	DString str;
	str.init(capacity, using);
	return str;
}

fn DString tnew_with_capacity(usz capacity) => new_with_capacity(capacity, mem::temp()) @inline;

fn DString new(String s = "", Allocator* using = mem::heap())
{
	DString str = new_with_capacity(s.len, using);
	if (s.len > 0)
	{
		StringData* data = str.data();
		data.len = s.len;
		mem::copy(data.start(), s.ptr, s.len);
	}
	return str;
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

fn DString new_join(String[] s, String joiner, Allocator* using = mem::heap())
{
	if (!s.len) return (DString)null;
	usz total_size = joiner.len * s.len;
	foreach (&str : s)
	{
		total_size += str.len;
	}
	DString res = new_with_capacity(total_size, using);
	res.append(s[0]);
	foreach (&str : s[1..])
	{
		res.append(joiner);
		res.append(*str);
	}
	return res;
}

fn String DString.str_view(self)
{
	StringData* data = self.data();
	if (!data) return "";
	char* start = data.start();
	return (String)start[:data.len];
}

fn ZString DString.zstr_view(&self)
{
	StringData* data = self.data();
	if (!data) return "";
	if (data.capacity == data.len)
	{
		data = self.reserve(1);
	}
	char* start = data.start();
	*(start + data.len) = 0;
	return (ZString)start;
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

fn void DString.clear(self)
{
	if (!self) return;
	self.data().len = 0;
}


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
	StringData* new_data = new_string.data();
	new_data.len = data.len;
	mem::copy(new_data.start(), data.start(), data.len);
	return new_string;
}

fn DString DString.tcopy(&self) => self.copy(mem::temp());

fn ZString DString.copy_zstr(self, Allocator* using = mem::heap())
{
	usz str_len = self.len();
	if (!str_len)
	{
		return (ZString)calloc(1, .using = using);
	}
	char* zstr = malloc(str_len + 1, .using = using);
	StringData* data = self.data();
	mem::copy(zstr, data.start(), str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn String DString.copy_str(self, Allocator* using = mem::heap())
{
	return (String)self.copy_zstr(using)[:self.len()];
}

fn String DString.tcopy_str(self) => self.copy_str(mem::temp()) @inline;


/**
 * @require index < self.len()
 **/
fn void DString.set(self, usz index, char c)
{
	StringData* data = self.data();
	char* start = data.start();
	*(start + index) = c;
}

fn void DString.append_repeat(&self, char c, usz times)
{
	if (times == 0) return;
	StringData* data = self.reserve(times);
	char* start = data.end();
	char* end = (void*)start + times;
	while (start < end)
	{
		*start = c;
		start++;
	}
	data.len += times;
}

fn void DString.append_char(&self, char c)
{
	if (!*self)
	{
		*self = new_with_capacity(MIN_CAPACITY);
	}
	StringData* data = self.reserve(1);
	char* end = data.end();
	*(end) = c;
	data.len++;
}

fn void DString.append_utf32(&self, Char32[] chars)
{
	StringData* data = self.reserve(4 * chars.len);
	char* end = data.end();
	foreach (c : chars)
	{
    	data.len += conv::char32_to_utf8_unsafe(c, &end);
	}
}

/**
 * @require c <= 0x10ffff
 */
fn void DString.append_char32(&self, Char32 c)
{
	char[4] buffer @noinit;
	char* p = &buffer;
	usz n = conv::char32_to_utf8_unsafe(c, &p);
	StringData* data = self.reserve(n);
	char* end = data.end();
	mem::copy(end, &buffer, n);
	data.len += n;
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
	StringData* data = self.reserve(other_len);
	char* end = data.end();
	mem::copy(end, str.ptr, other_len);
	data.len += other_len;
}

fn void DString.append_string(&self, DString str)
{
	self.append(str.str_view());
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


fn void DString.insert_at(&self, usz index, String s)
{
	if (s.len == 0) return;
	self.reserve(s.len);
	StringData* data = self.data();
	char* start = data.start();
	if (start == s.ptr)
	{
		// Source and destination are the same: nothing to do.
		return;
	}
	usz len = data.len;
	index = min(index, len);
	data.len += s.len;

	void* p = (void*)start + index;
	mem::move(p + s.len, p, len - index); // move existing data
	switch
	{
		case s.ptr <= p && p < (void*)s.ptr + s.len:
			// Overlapping areas.
			foreach_r (i, c : s)
			{
				start[index + i] = c;
			}
		case p <= s.ptr && s.ptr < p + len:
			// Source has moved.
			mem::move(p, (void*)s.ptr + s.len, s.len);
		default:
			mem::move(p, s, s.len);
	}
}


fn bool DString.equals(self, DString other_string)
{
	StringData *str1 = self.data();
	StringData *str2 = other_string.data();
	if (str1 == str2) return true;
	if (!str1) return str2.len == 0;
	if (!str2) return str1.len == 0;
	if (str1.len != str2.len) return false;
	char* start1 = str1.start();
	char* start2 = str2.start();
	char* end = str1.end();
	while (start1 < end)
	{
		if (*start1 != *start2) return false;
		start1++;
		start2++;
	}
	return true;
}

fn bool DString.less(self, DString other_string)
{
	StringData* str1 = self.data();
	StringData* str2 = other_string.data();
	if (str1 == str2) return false;
	if (!str1) return str2.len != 0;
	if (!str2) return str1.len == 0;
	if (str1.len != str2.len) return str1.len < str2.len;
	char* start1 = str1.start();
	char* start2 = str2.start();
	char* end = str1.end();
	while (start1 < end)
	{
		if (*start1 > *start2) return false;
		start1++;
		start2++;
	}
	return true;
}


fn usz! DString.printf(&self, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn2, self);
	return formatter.vprintf(format, args);
}

fn usz! DString.printfn(&self, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_string_append_fn2, self);
	usz len = formatter.vprintf(format, args)!;
	self.append('\n');
	return len + 1;
}

fn usz! DString.read_from_stream(&self, Stream* reader)
{
	if (reader.supports_available())
	{
		usz total_read = 0;
		while (usz available = reader.available()!)
		{
			StringData* data = self.reserve(available);
			char* start = data.start();
			usz len = reader.read(start[data.len..data.capacity - 1])!;
			total_read += len;
			data.len += len;
		}
		return total_read;
	}
	usz total_read = 0;
	while (true)
	{
		// Reserve at least 16 bytes
		StringData* data = self.reserve(MIN_CAPACITY);
		// Read into the rest of the buffer
		char* start = data.start();
		usz read = reader.read(start[data.len..data.capacity - 1])!;
		data.len += read;
		// Ok, we reached the end.
		if (read < MIN_CAPACITY) return total_read;
		// Otherwise go another round
	}
}



fn StringData* DString.data(self) @inline @private
{
	return (StringData*)self;
}

fn StringData* DString.reserve(&self, usz addition)
{
	StringData* data = self.data();
	if (!data)
	{
		*self = new_with_capacity(addition);
		return self.data();
	}
	assert(data.capacity >= data.len);
	if (data.capacity - data.len >= addition) return data;
	usz len = data.len;
	usz new_capacity = data.capacity * 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	usz new_len = len + addition;
	while (new_capacity < new_len) new_capacity *= 2;

	char* start = data.start();
	Allocator* using = data.allocator ? data.allocator : mem::heap();
	char* p = (char*)realloc(start, new_capacity + StringData.sizeof, .using = using);
	*self = get_dstring(p, len, new_capacity, using);

	return self.data();
}

macro DString get_dstring(char* p, len, capacity, using)
{
	StringData* data = (StringData*)((void*)p + capacity);
	*data = { .allocator = using, .len = len, .capacity = capacity };
	return (DString)data;
}

fn void! out_string_append_fn2(void* data, char c) @private
{
	DString* s = data;
	s.append_char(c);
}
