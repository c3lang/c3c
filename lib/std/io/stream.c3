module std::io;

def CloseStreamFn = fn void!(Stream*);
def FlushStreamFn = fn void!(Stream*);
def SeekStreamFn = fn usz!(Stream*, isz offset, Seek seek);
def LenStreamFn = fn usz(Stream*);
def AvailableStreamFn = fn usz!(Stream*);
def ReadStreamFn = fn usz!(Stream*, char[] bytes);
def ReadFromStreamFn = fn usz!(Stream*, Stream*);
def ReadByteStreamFn = fn char!(Stream*);
def PushbackByteStreamFn = fn void!(Stream*);
def WriteStreamFn = fn usz!(Stream*, char[] bytes);
def WriteToStreamFn = fn usz!(Stream*, Stream* out);
def WriteByteStreamFn = fn void!(Stream*, char c);
def DestroyStreamFn = fn void!(Stream*);

struct StreamInterface
{
	CloseStreamFn close_fn;
	FlushStreamFn flush_fn;
	SeekStreamFn seek_fn;
	LenStreamFn len_fn;
	AvailableStreamFn available_fn;
	ReadStreamFn read_fn;
	ReadFromStreamFn read_stream_fn;
	ReadByteStreamFn read_byte_fn;
	PushbackByteStreamFn pushback_byte_fn;
	WriteStreamFn write_fn;
	WriteToStreamFn write_stream_fn;
	WriteByteStreamFn write_byte_fn;
	DestroyStreamFn destroy_fn;
}

struct Stream
{
	StreamInterface* fns;
}

struct StreamWrapper
{
	inline Stream stream;
	void* data;
}

fn bool Stream.supports_flush(&s) @inline => (bool)s.fns.flush_fn;
fn bool Stream.supports_seek(&s) @inline => (bool)s.fns.seek_fn;
fn bool Stream.supports_available(&s) @inline => s.fns.available_fn || s.fns.seek_fn;
fn bool Stream.supports_len(&s) @inline => s.fns.len_fn || s.fns.seek_fn;
fn bool Stream.supports_read(&s) @inline => s.fns.read_fn || s.fns.read_byte_fn;
fn bool Stream.supports_read_from(&s) @inline => (bool)s.fns.read_stream_fn;
fn bool Stream.supports_write_to(&s) @inline => (bool)s.fns.write_stream_fn;
fn bool Stream.supports_pushback_byte(&s) @inline => s.fns.pushback_byte_fn || s.fns.seek_fn;
fn bool Stream.supports_write(&s) @inline => s.fns.write_fn || s.fns.write_byte_fn;
fn bool Stream.supports_read_byte(&s) @inline => (bool)s.fns.read_byte_fn;
fn bool Stream.supports_write_byte(&s) @inline => (bool)s.fns.write_byte_fn;

fn void! Stream.destroy(&self) @inline @maydiscard
{
	if (self.fns.destroy_fn) return self.fns.destroy_fn(self);
	return self.close();
}

fn void! Stream.close(&self) @inline @maydiscard
{
	if (CloseStreamFn func = self.fns.close_fn) return func(self);
}

fn usz! Stream.seek(&self, isz offset, Seek seek) @inline
{
	if (SeekStreamFn func = self.fns.seek_fn) return func(self, offset, seek);
	return IoError.NOT_SEEKABLE?;
}

fn usz! Stream.available(&self) @inline
{
	if (AvailableStreamFn func = self.fns.available_fn) return func(self);
	if (SeekStreamFn func = self.fns.seek_fn)
	{
		usz curr = func(self, 0, Seek.CURSOR)!;
		usz len = func(self, 0, Seek.END)!;
		func(self, curr, Seek.SET)!;
		return len - curr;
	}
	return IoError.NOT_SEEKABLE?;
}

fn usz! Stream.read_any(&self, any* ref)
{
	return self.read_all(ref.ptr[:ref.type.sizeof]);
}

/**
 * @param ref "the object to write."
 * @require ref.ptr != null
 * @ensure return == ref.type.sizeof
 */
fn usz! Stream.write_any(&self, any* ref)
{
	return self.write_all(ref.ptr[:ref.type.sizeof]);
}

fn usz! Stream.read(&self, char[] buffer)
{
	if (ReadStreamFn func = self.fns.read_fn) return func(self, buffer);
	if (ReadByteStreamFn func = self.fns.read_byte_fn)
	{
		usz len = 0;
		foreach (&cptr : buffer)
		{
			char! c = func(self);
			if (catch err = c)
			{
				case IoError.EOF: return len;
				default: return err?;
			}
			*cptr = c;
			len++;
		}
	}
	return IoError.UNSUPPORTED_OPERATION?;
}

macro usz! Stream.print(&self, x) @maydiscard
{
	return print_gen(self, x);
}

macro usz! print_gen(self, x)
{
	var $Type = $typeof(x);
	$switch ($Type)
	$case String:
		return self.write(x);
	$case ZString:
		return self.write(x.str_view());
	$case DString:
		return self.write(x.str_view());
	$default:
		$if $defined((String)x):
			return self.write((String)x);
		$else
			return printf("%s", x);
		$endif
	$endswitch
}

macro usz! printn_gen(self, x) @maydiscard
{
	usz len = print_gen(self, x)!;
	self.write_byte('\n')!;
	self.flush()!;
	return len + 1;
}

macro usz! Stream.printn(&self, x) @maydiscard
{
	return printn_gen(self, x);
}

fn usz! Stream.printf(&self, String format, args...) @maydiscard
{
	Formatter formatter;
	if (WriteByteStreamFn func_byte = self.fns.write_byte_fn)
	{
		formatter.init((OutputFn)func_byte, self);
	}
	else if (WriteStreamFn func = self.fns.write_fn)
	{
		formatter.init((OutputFn)&Stream.write_byte, self);
	}
	else
	{
		return IoError.UNSUPPORTED_OPERATION?;
	}
	return formatter.vprintf(format, args)!;
}

fn usz! Stream.printfn(&self, String format, args...) @maydiscard @inline
{
	Formatter formatter;
	if (WriteByteStreamFn func_byte = self.fns.write_byte_fn)
	{
		formatter.init((OutputFn)func_byte, self);
	}
	else if (WriteStreamFn func = self.fns.write_fn)
	{
		formatter.init((OutputFn)&Stream.write_byte, self);
	}
	else
	{
		return IoError.UNSUPPORTED_OPERATION?;
	}
	usz len = formatter.vprintf(format, args)!;
	self.write_byte('\n')!;
	self.flush()!;
	return len + 1;
}

fn char! Stream.read_byte(&self) @inline
{
	if (ReadByteStreamFn func = self.fns.read_byte_fn) return func(self);
	if (ReadStreamFn func = self.fns.read_fn)
	{
		char[1] buffer;
		usz read = func(self, &buffer)!;
		if (read != 1) return IoError.EOF?;
		return buffer[0];
	}
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! Stream.read_all(&self, char[] buffer) @inline
{
	usz n = self.read(buffer)!;
	if (n != buffer.len) return IoError.UNEXPECTED_EOF?;
	return n;
}

fn usz! Stream.write_all(&self, char[] buffer) @inline
{
	usz n = self.write(buffer)!;
	if (n != buffer.len) return IoError.INCOMPLETE_WRITE?;
	return n;
}


fn String! Stream.treadline(&self) => self.readline(mem::temp()) @inline;

fn String! Stream.readline(&self, Allocator* using = mem::heap())
{
	if (ReadByteStreamFn func = self.fns.read_byte_fn)
	{
		char val = func(self)!;
    	if (val == '\n') return "";
    	@pool(using)
    	{
    		DString str = dstring::tnew_with_capacity(256);
    		if (val != '\r') str.append(val);
    		while (1)
    		{
    			char! c = func(self);
    			if (catch err = c)
    			{
    				if (err == IoError.EOF) break;
    				return err?;
    			}
    			if (c == '\r') continue;
    			if (c == '\n') break;
    			str.append_char(c);
    		}
    		return str.copy_str(using);
    	};
	}
	if (ReadStreamFn func = self.fns.read_fn)
	{
		char[1] buff;
		if (func(self, &buff)! == 0) return IoError.EOF?;
    	char val = buff[0];
		if (val == '\n') return "";
    	@pool(using)
    	{
    		DString str = dstring::tnew_with_capacity(256);
    		if (val != '\r') str.append(val);
    		while (1)
    		{
    		    usz! read = func(self, &buff);
    			if (catch err = read)
    			{
    				if (err == IoError.EOF) break;
    				return err?;
    			}
    		    if (!read) break;
    		    char c = buff[0];
    			if (c == '\r') continue;
    			if (c == '\n') break;
    			str.append_char(c);
    		}
    		return str.copy_str(using);
    	};
	}
	return IoError.UNSUPPORTED_OPERATION?;


}

fn usz! Stream.write(&self, char[] bytes) @inline
{
	if (WriteStreamFn func = self.fns.write_fn) return func(self, bytes);
	if (WriteByteStreamFn func = self.fns.write_byte_fn)
	{
		foreach (c : bytes) func(self, c)!;
		return bytes.len;
	}
	return IoError.UNSUPPORTED_OPERATION?;
}

fn void! Stream.write_byte(&self, char b) @inline
{
	if (WriteByteStreamFn func = self.fns.write_byte_fn) return func(self, b);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! Stream.write_to(&self, Stream* to) @inline
{
	if (WriteToStreamFn func = self.fns.write_stream_fn) return func(self, to);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! Stream.read_from(&self, Stream* from) @inline
{
	if (ReadFromStreamFn func = self.fns.read_stream_fn) return func(self, from);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn void! Stream.flush(&self) @inline @maydiscard
{
	if (FlushStreamFn func = self.fns.flush_fn) return func(self);
	return IoError.UNSUPPORTED_OPERATION?;
}

fn usz! Stream.len(&self) @inline
{
	if (LenStreamFn func = self.fns.len_fn) return func(self);
	if (SeekStreamFn func = self.fns.seek_fn)
	{
		usz curr = func(self, 0, Seek.CURSOR)!;
		usz len = func(self, 0, Seek.END)!;
		func(self, curr, Seek.SET)!;
		return len;
	}
	return IoError.NOT_SEEKABLE?;
}

fn void! Stream.pushback_byte(&self) @inline
{
	if (PushbackByteStreamFn func = self.fns.pushback_byte_fn) return func(self);
	if (SeekStreamFn func = self.fns.seek_fn)
	{
		func(self, -1, CURSOR)!;
		return;
	}
	return IoError.UNSUPPORTED_OPERATION?;
}

fn void! Stream.write_string(&self, String str) @inline => (void)(self.write((char[])str)!);

fn usz! Stream.copy_to(&self, Stream* dst, char[] buffer = {})
{
	if (buffer.len) return copy_through_buffer(self, dst, buffer);
	if (WriteToStreamFn func = self.fns.write_stream_fn) return func(self, dst);
	if (ReadFromStreamFn func = self.fns.read_stream_fn) return func(dst, self);
	$switch (env::MEMORY_ENV)
		$case NORMAL:
			@pool()
			{
				return copy_through_buffer(self, dst, tmalloc(char, 4096));
			};
		$case SMALL:
			@pool()
			{
				return copy_through_buffer(self, dst, tmalloc(char, 1024));
			};
		$case TINY:
		$case NONE:
			return copy_through_buffer(self, dst, &&(char[256]{}));
	$endswitch
}

macro usz! copy_through_buffer(Stream *self, Stream* dst, char[] buffer) @local
{
	usz total_copied;
	while (true)
	{
		usz! len = self.read(buffer);
		if (catch err = len)
		{
			case IoError.EOF: return total_copied;
			default: return err?;
		}
		if (!len) return total_copied;
		usz written = dst.write(buffer[:len])!;
		total_copied += len;
		if (written != len) return IoError.INCOMPLETE_WRITE?;
	}
}

const char[*] MAX_VARS @private = { [2] = 3, [4] = 5, [8] = 10 };

/**
 * @require $typeof(x_ptr).kindof == POINTER && $typeof(x_ptr).inner.kindof.is_int()
 **/
macro usz! Stream.read_varint(&self, x_ptr)
{
	var $Type = $typefrom($typeof(x_ptr).inner);
	const MAX = MAX_VARS[$Type.sizeof];
	$Type x;
	uint shift;
	usz n;
	for (usz i = 0; i < MAX; i++)
	{
		char! c = self.read_byte();
		if (catch err = c)
		{
			case IoError.EOF:
				return IoError.UNEXPECTED_EOF?;
			default:
				return err?;
		}
		n++;
		if (c & 0x80 == 0)
		{
			if (i + 1 == MAX && c > 1) break;
			x |= c << shift;
			$if $Type.kindof == SIGNED_INT:
				x = x & 1 == 0 ? x >> 1 : ~(x >> 1);
			$endif
			*x_ptr = x;
			return n;
		}
		x |= (c & 0x7F) << shift;
		shift += 7;
	}
	return MathError.OVERFLOW?;
}

/**
 * @require $typeof(x).kindof.is_int()
 **/
macro usz! Stream.write_varint(&self, x)
{
	var $Type = $typeof(x);
	const MAX = MAX_VARS[$Type.sizeof];
	char[MAX] buffer @noinit;
	usz i;
	while (x >= 0x80)
	{
		buffer[i] = (char)(x | 0x80);
		x >>= 7;
		i++;
	}
	buffer[i] = (char)x;
	return self.write_all(buffer[:i + 1]);
}