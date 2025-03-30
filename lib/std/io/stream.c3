module std::io;
import std::math;

interface InStream
{
	fn void? close() @optional;
	fn usz? seek(isz offset, Seek seek) @optional;
	fn usz len() @optional;
	fn usz? available() @optional;
	fn usz? read(char[] buffer);
	fn char? read_byte();
	fn usz? write_to(OutStream out) @optional;
	fn void? pushback_byte() @optional;
}


interface OutStream
{
	fn void? destroy() @optional;
	fn void? close() @optional;
	fn void? flush() @optional;
	fn usz? write(char[] bytes);
	fn void? write_byte(char c);
	fn usz? read_to(InStream in) @optional;
}

fn usz? available(InStream s)
{
	if (&s.available) return s.available();
	if (&s.seek)
	{
		usz curr = s.seek(0, Seek.CURSOR)!;
		usz len = s.seek(0, Seek.END)!;
		s.seek(curr, Seek.SET)!;
		return len - curr;
	}
	return 0;
}

macro bool @is_instream(#expr)
{
	return $assignable(#expr, InStream);
}

macro bool @is_outstream(#expr)
{
	return $assignable(#expr, OutStream);
}

<*
 @param [&out] ref
 @require @is_instream(stream)
*>
macro usz? read_any(stream, any ref)
{
	return read_all(stream, ((char*)ref)[:ref.type.sizeof]);
}

<*
 @param [&in] ref : "the object to write."
 @require @is_outstream(stream)
 @ensure return == ref.type.sizeof
*>
macro usz? write_any(stream, any ref)
{
	return write_all(stream, ((char*)ref)[:ref.type.sizeof]);
}

<*
 @require @is_instream(stream)
*>
macro usz? read_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	usz n = stream.read(buffer)!;
	if (n != buffer.len) return UNEXPECTED_EOF?;
	return n;
}

<*
 @require @is_instream(stream)
*>
macro char[]? read_fully(Allocator allocator, stream)
{
	usz len = available(stream)!;
	char* data = allocator::malloc_try(allocator, len)!;
	defer catch allocator::free(allocator, data);
	usz read = 0;
	while (read < len)
	{
		read += stream.read(data[read:len - read])!;
	}
	return data[:len];
}

<*
 @require @is_outstream(stream)
*>
macro usz? write_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	usz n = stream.write(buffer)!;
	if (n != buffer.len) return INCOMPLETE_WRITE?;
	return n;
}

macro usz? read_using_read_byte(s, char[] buffer)
{
	usz len = 0;
	foreach (&cptr : buffer)
	{
		char? c = s.read_byte();
		if (catch err = c)
		{
			if (err == io::EOF) return len;
			return err?;
		}
		*cptr = c;
		len++;
	}
	return len;
}

macro void? write_byte_using_write(s, char c)
{
	char[1] buff = { c };
	s.write(&buff)!;
}

macro char? read_byte_using_read(s)
{
	char[1] buffer;
	usz read = s.read(&buffer)!;
	if (read != 1) return io::EOF?;
	return buffer[0];
}

alias ReadByteFn = fn char?();


macro usz? write_using_write_byte(s, char[] bytes)
{
	foreach (c : bytes) s.write_byte(self, c)!;
	return bytes.len;
}

macro void? pushback_using_seek(s)
{
	s.seek(-1, CURSOR)!;
}

fn usz? copy_to(InStream in, OutStream dst, char[] buffer = {})
{
	if (buffer.len) return copy_through_buffer(in, dst, buffer);
	if (&in.write_to) return in.write_to(dst);
	if (&dst.read_to) return dst.read_to(in);
	$switch env::MEMORY_ENV:
		$case NORMAL:
			return copy_through_buffer(in, dst, &&(char[4096]){});
		$case SMALL:
			return copy_through_buffer(in, dst, &&(char[1024]){});
		$case TINY:
		$case NONE:
			return copy_through_buffer(in, dst, &&(char[256]){});
	$endswitch
}

macro usz? copy_through_buffer(InStream in, OutStream dst, char[] buffer) @local
{
	usz total_copied;
	while (true)
	{
		usz? len = in.read(buffer);
		if (catch err = len)
		{
			if (err == io::EOF) return total_copied;
			return err?;
		}
		if (!len) return total_copied;
		usz written = dst.write(buffer[:len])!;
		total_copied += len;
		if (written != len) return INCOMPLETE_WRITE?;
	}
}

const char[*] MAX_VARS @private = { [2] = 3, [4] = 5, [8] = 10 };

<*
 @require @is_instream(stream)
 @require @typekind(x_ptr) == POINTER && $typeof(x_ptr).inner.kindof.is_int()
*>
macro usz? read_varint(stream, x_ptr)
{
	var $Type = $typefrom($typeof(x_ptr).inner);
	const MAX = MAX_VARS[$Type.sizeof];
	$Type x;
	uint shift;
	usz n;
	for (usz i = 0; i < MAX; i++)
	{
		char? c = stream.read_byte();
		if (catch err = c)
		{
			if (err == io::EOF) return io::UNEXPECTED_EOF?;
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
	return math::OVERFLOW?;
}
<*
 @require @is_outstream(stream)
 @require @typekind(x).is_int()
*>
macro usz? write_varint(stream, x)
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
	return write_all(stream, buffer[:i + 1]);
}

<*
 @require @is_instream(stream)
*>
macro ushort? read_be_ushort(stream)
{
	char hi_byte = stream.read_byte()!;
	char lo_byte = stream.read_byte()!;
	return (ushort)(hi_byte << 8 | lo_byte);
}

<*
 @require @is_instream(stream)
*>
macro short? read_be_short(stream)
{
	return read_be_ushort(stream);
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_short(stream, ushort s)
{
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_instream(stream)
*>
macro uint? read_be_uint(stream)
{
	uint val = stream.read_byte()! << 24;
	val += stream.read_byte()! << 16;
	val += stream.read_byte()! << 8;
	return val + stream.read_byte()!;
}

<*
 @require @is_instream(stream)
*>
macro int? read_be_int(stream)
{
	return read_be_uint(stream);
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_int(stream, uint s)
{
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_instream(stream)
*>
macro ulong? read_be_ulong(stream)
{
	ulong val = (ulong)stream.read_byte()! << 56;
	val += (ulong)stream.read_byte()! << 48;
	val += (ulong)stream.read_byte()! << 40;
	val += (ulong)stream.read_byte()! << 32;
	val += (ulong)stream.read_byte()! << 24;
	val += (ulong)stream.read_byte()! << 16;
	val += (ulong)stream.read_byte()! << 8;
	return val + stream.read_byte()!;
}

<*
 @require @is_instream(stream)
*>
macro long? read_be_long(stream)
{
	return read_be_ulong(stream);
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_long(stream, ulong s)
{
	stream.write_byte((char)(s >> 56))!;
	stream.write_byte((char)(s >> 48))!;
	stream.write_byte((char)(s >> 40))!;
	stream.write_byte((char)(s >> 32))!;
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_instream(stream)
*>
macro uint128? read_be_uint128(stream)
{
	uint128 val = (uint128)stream.read_byte()! << 120;
	val += (uint128)stream.read_byte()! << 112;
	val += (uint128)stream.read_byte()! << 104;
	val += (uint128)stream.read_byte()! << 96;
	val += (uint128)stream.read_byte()! << 88;
	val += (uint128)stream.read_byte()! << 80;
	val += (uint128)stream.read_byte()! << 72;
	val += (uint128)stream.read_byte()! << 64;
	val += (uint128)stream.read_byte()! << 56;
	val += (uint128)stream.read_byte()! << 48;
	val += (uint128)stream.read_byte()! << 40;
	val += (uint128)stream.read_byte()! << 32;
	val += (uint128)stream.read_byte()! << 24;
	val += (uint128)stream.read_byte()! << 16;
	val += (uint128)stream.read_byte()! << 8;
	return val + stream.read_byte()!;
}

<*
 @require @is_instream(stream)
*>
macro int128? read_be_int128(stream)
{
	return read_be_uint128(stream);
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_int128(stream, uint128 s)
{
	stream.write_byte((char)(s >> 120))!;
	stream.write_byte((char)(s >> 112))!;
	stream.write_byte((char)(s >> 104))!;
	stream.write_byte((char)(s >> 96))!;
	stream.write_byte((char)(s >> 88))!;
	stream.write_byte((char)(s >> 80))!;
	stream.write_byte((char)(s >> 72))!;
	stream.write_byte((char)(s >> 64))!;
	stream.write_byte((char)(s >> 56))!;
	stream.write_byte((char)(s >> 48))!;
	stream.write_byte((char)(s >> 40))!;
	stream.write_byte((char)(s >> 32))!;
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_outstream(stream)
 @require data.len < 256 : "Data exceeded 255"
*>
macro usz? write_tiny_bytearray(stream, char[] data)
{
	stream.write_byte((char)data.len)!;
	return stream.write(data) + 1;
}

<*
 @require @is_instream(stream)
*>
macro char[]? read_tiny_bytearray(stream, Allocator allocator)
{
	int len = stream.read_byte()!;
	if (!len) return {};
	char[] data = allocator::alloc_array(allocator, char, len);
	io::read_all(stream, data)!;
	return data;
}

<*
 @require @is_outstream(stream)
 @require data.len < 0x1000 : "Data exceeded 65535"
*>
macro usz? write_short_bytearray(stream, char[] data)
{
	io::write_be_short(stream, (ushort)data.len)!;
	return stream.write(data) + 2;
}

<*
 @require @is_instream(stream)
*>
macro char[]? read_short_bytearray(stream, Allocator allocator)
{
	int len = io::read_be_ushort(stream)!;
	if (!len) return {};
	char[] data = allocator::alloc_array(allocator, char, len);
	io::read_all(stream, data)!;
	return data;
}

<*
 Wrap bytes for reading using io functions.
*>
fn ByteReader wrap_bytes(char[] bytes)
{
	return { bytes, 0 };
}