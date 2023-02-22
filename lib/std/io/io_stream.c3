module std::io;

typedef CloseStreamFn = fn void!(Stream*);
typedef FlushStreamFn = fn void!(Stream*);
typedef SeekStreamFn = fn usz!(Stream*, isz offset, Seek seek);
typedef LenStreamFn = fn usz(Stream*);
typedef AvailableStreamFn = fn usz(Stream*);
typedef ReadStreamFn = fn usz!(Stream*, char[] bytes);
typedef ReadFromStreamFn = fn usz!(Stream*, Stream*);
typedef ReadByteStreamFn = fn char!(Stream*);
typedef PushbackByteStreamFn = fn void!(Stream*);
typedef WriteStreamFn = fn usz!(Stream*, char[] bytes);
typedef WriteToStreamFn = fn usz!(Stream*, Stream* out);
typedef WriteByteStreamFn = fn void!(Stream*, char c);
typedef DestroyStreamFn = fn void!(Stream*);

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
	StreamInterface *fns;
	void* data;
}

fn bool Stream.supports_seek(Stream* s) @inline => (bool)s.fns.seek_fn;
fn bool Stream.supports_available(Stream* s) @inline => s.fns.available_fn || s.fns.seek_fn;
fn bool Stream.supports_len(Stream* s) @inline => s.fns.len_fn || s.fns.seek_fn;
fn bool Stream.supports_read(Stream* s) @inline => s.fns.read_fn || s.fns.read_byte_fn;
fn bool Stream.supports_read_from(Stream* s) @inline => (bool)s.fns.read_stream_fn;
fn bool Stream.supports_write_to(Stream* s) @inline => (bool)s.fns.write_stream_fn;
fn bool Stream.supports_pushback_byte(Stream* s) @inline => s.fns.pushback_byte_fn || s.fns.seek_fn;
fn bool Stream.supports_write(Stream* s) @inline => s.fns.write_fn || s.fns.write_byte_fn;

fn void! Stream.destroy(Stream* s) @inline @maydiscard
{
	if (s.fns.destroy_fn) return s.fns.destroy_fn(s);
	return s.close();
}

fn void! Stream.close(Stream* s) @inline @maydiscard
{
	if (CloseStreamFn func = s.fns.close_fn) return func(s);
}

fn usz! Stream.seek(Stream* s, isz offset, Seek seek) @inline
{
	if (SeekStreamFn func = s.fns.seek_fn) return func(s, offset, seek);
	return IoError.NOT_SEEKABLE!;
}

fn usz! Stream.available(Stream* s) @inline
{
	if (AvailableStreamFn func = s.fns.available_fn) return func(s);
	if (SeekStreamFn func = s.fns.seek_fn)
	{
		usz curr = func(s, 0, Seek.CURSOR)?;
		usz len = func(s, 0, Seek.END)?;
		func(s, curr, Seek.SET)?;
		return len - curr;
	}
	return IoError.NOT_SEEKABLE!;
}

fn usz! Stream.read(Stream* s, char[] buffer)
{
	if (ReadStreamFn func = s.fns.read_fn) return func(s, buffer);
	if (ReadByteStreamFn func = s.fns.read_byte_fn)
	{
		usz len = 0;
		foreach (&cptr : buffer)
		{
			char! c = func(s);
			if (catch err = c)
			{
				case IoError.EOF: return len;
				default: return err!;
			}
			*cptr = c;
			len++;
		}
	}
	return IoError.UNSUPPORTED_OPERATION!;
}

fn char! Stream.read_byte(Stream* s) @inline
{
	if (ReadByteStreamFn func = s.fns.read_byte_fn) return func(s);
	return IoError.UNSUPPORTED_OPERATION!;
}

fn usz! Stream.write(Stream* s, char[] bytes) @inline
{
	if (WriteStreamFn func = s.fns.write_fn) return func(s, bytes);
	if (WriteByteStreamFn func = s.fns.write_byte_fn)
	{
		foreach (c : bytes) func(s, c)?;
		return bytes.len;
	}
	return IoError.UNSUPPORTED_OPERATION!;
}

fn void! Stream.write_byte(Stream* s, char b) @inline
{
	if (WriteByteStreamFn func = s.fns.write_byte_fn) return func(s, b);
	return IoError.UNSUPPORTED_OPERATION!;
}

fn usz! Stream.write_to(Stream* s, Stream* to) @inline
{
	if (WriteToStreamFn func = s.fns.write_stream_fn) return func(s, to);
	return IoError.UNSUPPORTED_OPERATION!;
}

fn usz! Stream.read_from(Stream* s, Stream* from) @inline
{
	if (ReadFromStreamFn func = s.fns.read_stream_fn) return func(s, from);
	return IoError.UNSUPPORTED_OPERATION!;
}

fn void! Stream.flush(Stream* s) @inline @maydiscard
{
	if (FlushStreamFn func = s.fns.flush_fn) return func(s);
	return IoError.UNSUPPORTED_OPERATION!;
}

fn usz! Stream.len(Stream* s) @inline
{
	if (LenStreamFn func = s.fns.len_fn) return func(s);
	if (SeekStreamFn func = s.fns.seek_fn)
	{
		usz curr = func(s, 0, Seek.CURSOR)?;
		usz len = func(s, 0, Seek.END)?;
		func(s, curr, Seek.SET)?;
		return len;
	}
	return IoError.NOT_SEEKABLE!;
}

fn void! Stream.pushback_byte(Stream* s) @inline
{
	if (PushbackByteStreamFn func = s.fns.pushback_byte_fn) return func(s);
	if (SeekStreamFn func = s.fns.seek_fn)
	{
		func(s, -1, CURSOR)?;
		return;
	}
	return IoError.UNSUPPORTED_OPERATION!;
}

fn void! Stream.write_string(Stream* s, String str) @inline => (void)(s.write((char[])str)?);

fn usz! Stream.copy_to(Stream* s, Stream* dst, char[] buffer = {})
{
	if (buffer.len) return copy_through_buffer(s, dst, buffer);
	if (WriteToStreamFn func = s.fns.write_stream_fn) return func(s, dst);
	if (ReadFromStreamFn func = dst.fns.read_stream_fn) return func(dst, s);
$switch (env::MEMORY_ENV):
$case NORMAL:
	@pool()
	{
		return copy_through_buffer(s, dst, array::talloc(char, 4096));
	};
$case SMALL:
	@pool()
	{
		return copy_through_buffer(s, dst, array::talloc(char, 1024));
	};
$case TINY:
$case NONE:
	return copy_through_buffer(s, dst, &&(char[256]{}));
$endswitch;
}

macro usz! copy_through_buffer(Stream* s, Stream* dst, char[] buffer) @local
{
	usz total_copied;
	while (true)
	{
		usz! len = s.read(buffer);
		if (catch err = len)
		{
			case IoError.EOF: return total_copied;
			default: return err!;
		}
		if (!len) return total_copied;
		usz written = dst.write(buffer[:len])?;
		total_copied += len;
		if (written != len) return IoError.INCOMPLETE_WRITE!;
	}
}
