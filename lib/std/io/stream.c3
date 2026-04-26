module std::io;
import std::math;



alias SeekFn = fn void?(void*, long offset, SeekOrigin whence = FROM_START);


interface InStream
{
	fn void? close() @optional;
	fn long? cursor() @optional;
	fn void? seek(long offset, SeekOrigin whence = FROM_START) @optional;
	fn long len() @optional;
	fn long? size() @optional;
	fn long? available() @optional;
	fn sz? read(char[] buffer);
	fn char? read_byte();
	fn long? write_to(OutStream out) @optional;
	fn void? pushback_byte() @optional;
}


interface OutStream
{
	fn void? destroy() @optional;
	fn void? close() @optional;
	fn void? flush() @optional;
	fn sz? write(char[] bytes);
	fn void? write_byte(char c);
	fn long? read_to(InStream in) @optional;
}

fn long? available(InStream s)
{
	if (&s.available) return s.available();
	if (&s.seek && &s.cursor)
	{
		long curr = s.cursor()!;
		s.seek(0, FROM_END)!;
		long len = s.cursor()!;
		s.seek(curr)!;
		return len - curr;
	}
	return io::UNSUPPORTED_OPERATION~;
}

macro bool @is_instream(#expr) @const
{
	return $defined(InStream i = #expr);
}

macro bool @is_not_instream_if_ptr(#expr) @const
{
	return !$defined(&#expr) ||| !@is_instream(&#expr);
}

macro bool @is_outstream(#expr) @const
{
	return $defined(OutStream s = #expr);
}

macro bool @is_not_outstream_if_ptr(#expr) @const
{
	return !$defined(&#expr) ||| !@is_outstream(&#expr);
}

<*
 @param [&out] ref
 @require @is_instream(stream) : "Expected a stream"
*>
macro sz? read_any(stream, any ref)
{
	return read_all(stream, ((char*)ref)[:ref.type.size]);
}

<*
 @param [&in] ref : "the object to write."
 @require @is_outstream(stream)
 @ensure return == ref.type.size
*>
macro sz? write_any(stream, any ref)
{
	return write_all(stream, ((char*)ref)[:ref.type.size]);
}

<*
 @require @is_instream(stream)
*>
macro sz? read_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	sz n = stream.read(buffer)!;
	if (n != buffer.len) return UNEXPECTED_EOF~;
	return n;
}

<*
 This function will read to the end of the stream.

 @require @is_instream(stream)
*>
macro char[]? read_fully(Allocator allocator, stream)
{
	// Efficient path if it is possible to pre-allocate
	if (try len = available(stream))
	{
		if (len > sz::max) return mem::INVALID_ALLOC_SIZE~;
		char* data = alloc::malloc_try(allocator, (sz)len)!;
		defer catch alloc::free(allocator, data);
		sz read = 0;
		while (read < len)
		{
			read += stream.read(data[read:len - read])!;
		}
		return data[:len];
	}
	ByteWriter writer;
	writer.init(allocator);
	copy_to(stream, &writer)!;
	return writer.array_view();
}


<*
 @require @is_outstream(stream)
*>
macro sz? write_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	sz n = stream.write(buffer)!;
	if (n != buffer.len) return INCOMPLETE_WRITE~;
	return n;
}

<*
 @require @is_instream(s)
*>
macro sz? read_using_read_byte(s, char[] buffer)
{
	sz len = 0;
	foreach (&cptr : buffer)
	{
		char? c = s.read_byte();
		if (catch err = c)
		{
			if (err == io::EOF) return len;
			return err~;
		}
		*cptr = c;
		len++;
	}
	return len;
}

<*
 @require @is_outstream(s)
*>
macro void? write_byte_using_write(s, char c)
{
	char[1] buff = { c };
	s.write(&buff)!;
}

<*
 @require @is_instream(s)
*>
macro char? read_byte_using_read(s)
{
	char[1] buffer;
	sz read = s.read(&buffer)!;
	if (read != 1) return io::EOF~;
	return buffer[0];
}

alias ReadByteFn = fn char?();

<*
 @require @is_outstream(s)
*>
macro sz? write_using_write_byte(s, char[] bytes)
{
	foreach (c : bytes) s.write_byte(c)!;
	return bytes.len;
}

macro void? pushback_using_seek(s)
{
	s.seek(-1, FROM_CURSOR)!;
}

fn long? copy_to(InStream in, OutStream dst, char[] buffer = {})
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

macro long? copy_through_buffer(InStream in, OutStream dst, char[] buffer) @local
{
	long total_copied;
	while (true)
	{
		sz? len = in.read(buffer);
		if (catch err = len)
		{
			if (err == io::EOF) return total_copied;
			return err~;
		}
		if (!len) return total_copied;
		sz written = dst.write(buffer[:len])!;
		total_copied += len;
		if (written != len) return INCOMPLETE_WRITE~;
	}
}

const char[*] MAX_VARS @private = { [1] = 2, [2] = 3, [4] = 5, [8] = 10, [16] = 19 };

<*
 @require @is_instream(stream)
 @require @kindof(x_ptr) == POINTER && @kindof(*x_ptr).is_int()
*>
macro sz? read_varint(stream, x_ptr)
{
	return read_varint_generic{$typeof(stream), $typeof(x_ptr)::inner}(stream, x_ptr);
}

fn sz? read_varint_generic(VarInStream stream, Type* ptr) <VarInStream, Type> @private
{
	const MAX = MAX_VARS[Type::size];
	Type x;
	uint shift;
	sz n;
	for (sz i = 0; i < MAX; i++)
	{
		char? c = stream.read_byte();
		if (catch err = c)
		{
			if (err == io::EOF) return io::UNEXPECTED_EOF~;
			return err~;
		}
		n++;
		if (c & 0x80 == 0)
		{
			if (i + 1 == MAX && c > 1) break;
			x |= (Type)c << shift;
			$if Type::kind == SIGNED_INT:
				x = x & 1 == 0 ? x >> 1 : ~(x >> 1);
			$endif
			*ptr = x;
			return n;
		}
		x |= (Type)(c & 0x7F) << shift;
		shift += 7;
	}
	return math::OVERFLOW~;
}


<*
 @require @is_outstream(stream)
 @require @kindof(x).is_int()
*>
macro sz? write_varint(stream, x)
{
	return write_varint_generic{$typeof(stream), $typeof(x)}(stream, x);
}

fn sz? write_varint_generic(VarOutStream stream, Type x) <VarOutStream, Type> @private
{
	const MAX = MAX_VARS[Type::size];
	char[MAX] buffer @noinit;
	sz i;
	while (x >= 0x80)
	{
		buffer[i] = (char)(x | 0x80);
		x >>= 7;
		i++;
	}
	buffer[i] = (char)x;
	return write_all(stream, buffer[:i + 1]);
}

<* @require @is_instream(stream) *>
macro short? read_be_short(stream) => (short)read_be_ushort(stream);
<* @require @is_instream(stream) *>
macro int? read_be_int(stream) => (int)read_be_uint(stream);
<* @require @is_instream(stream) *>
macro long? read_be_long(stream) => (long)read_be_ulong(stream);
<* @require @is_instream(stream) *>
macro int128? read_be_int128(stream) => (int128)read_be_uint128(stream);

<* @require @is_instream(stream) *>
macro short? read_le_short(stream) => (short)read_le_ushort(stream);
<* @require @is_instream(stream) *>
macro int? read_le_int(stream) => (int)read_le_uint(stream);
<* @require @is_instream(stream) *>
macro long? read_le_long(stream) => (long)read_le_ulong(stream);
<* @require @is_instream(stream) *>
macro int128? read_le_int128(stream) => (int128)read_le_uint128(stream);


<* @require @is_outstream(stream) *>
macro void? write_be_short(stream, short s) => write_be_ushort(stream, (ushort)s);
<* @require @is_outstream(stream) *>
macro void? write_be_int(stream, int s) => write_be_uint(stream, (uint)s);
<* @require @is_outstream(stream) *>
macro void? write_be_long(stream, long s) => write_be_ulong(stream, (ulong)s);
<* @require @is_outstream(stream) *>
macro void? write_be_int128(stream, int128 s) => write_be_uint128(stream, (uint128)s);

<* @require @is_outstream(stream) *>
macro void? write_le_short(stream, short s) => write_le_ushort(stream, (ushort)s);
<* @require @is_outstream(stream) *>
macro void? write_le_int(stream, int s) => write_le_uint(stream, (uint)s);
<* @require @is_outstream(stream) *>
macro void? write_le_long(stream, long s) => write_le_ulong(stream, (ulong)s);
<* @require @is_outstream(stream) *>
macro void? write_le_int128(stream, int128 s) => write_le_uint128(stream, (uint128)s);

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
macro ushort? read_le_ushort(stream)
{
	char lo_byte = stream.read_byte()!;
	char hi_byte = stream.read_byte()!;
	return (ushort)(hi_byte << 8 | lo_byte);
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_ushort(stream, ushort s)
{
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_outstream(stream)
*>
macro void? write_le_ushort(stream, ushort s)
{
	stream.write_byte((char)s)!;
	stream.write_byte((char)(s >> 8))!;
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
macro uint? read_le_uint(stream)
{
	uint val = stream.read_byte()!;
	val += stream.read_byte()! << 8;
	val += stream.read_byte()! << 16;
	return val + stream.read_byte()! << 24;
}


<*
 @require @is_outstream(stream)
*>
macro void? write_be_uint(stream, uint s)
{
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)s)!;
}

<*
 @require @is_outstream(stream)
*>
macro void? write_le_uint(stream, uint s)
{
	stream.write_byte((char)s)!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 24))!;
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
macro ulong? read_le_ulong(stream)
{
	ulong val = (ulong)stream.read_byte()!;
	val += (ulong)stream.read_byte()! << 8;
	val += (ulong)stream.read_byte()! << 16;
	val += (ulong)stream.read_byte()! << 24;
	val += (ulong)stream.read_byte()! << 32;
	val += (ulong)stream.read_byte()! << 40;
	val += (ulong)stream.read_byte()! << 48;
	return val + (ulong)stream.read_byte()! << 56;
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_ulong(stream, ulong s)
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
 @require @is_outstream(stream)
*>
macro void? write_le_ulong(stream, ulong s)
{
	stream.write_byte((char)s)!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 32))!;
	stream.write_byte((char)(s >> 40))!;
	stream.write_byte((char)(s >> 48))!;
	stream.write_byte((char)(s >> 56))!;
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
macro uint128? read_le_uint128(stream)
{
	uint128 val = stream.read_byte()!;
	val += (uint128)stream.read_byte()! << 8;
	val += (uint128)stream.read_byte()! << 16;
	val += (uint128)stream.read_byte()! << 24;
	val += (uint128)stream.read_byte()! << 32;
	val += (uint128)stream.read_byte()! << 40;
	val += (uint128)stream.read_byte()! << 48;
	val += (uint128)stream.read_byte()! << 56;
	val += (uint128)stream.read_byte()! << 64;
	val += (uint128)stream.read_byte()! << 72;
	val += (uint128)stream.read_byte()! << 80;
	val += (uint128)stream.read_byte()! << 88;
	val += (uint128)stream.read_byte()! << 96;
	val += (uint128)stream.read_byte()! << 104;
	val += (uint128)stream.read_byte()! << 112;
	return val + (uint128)stream.read_byte()! << 120;
}

<*
 @require @is_outstream(stream)
*>
macro void? write_be_uint128(stream, uint128 s)
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
*>
macro void? write_le_uint128(stream, uint128 s)
{
	stream.write_byte((char)s)!;
	stream.write_byte((char)(s >> 8))!;
	stream.write_byte((char)(s >> 16))!;
	stream.write_byte((char)(s >> 24))!;
	stream.write_byte((char)(s >> 32))!;
	stream.write_byte((char)(s >> 40))!;
	stream.write_byte((char)(s >> 48))!;
	stream.write_byte((char)(s >> 56))!;
	stream.write_byte((char)(s >> 64))!;
	stream.write_byte((char)(s >> 72))!;
	stream.write_byte((char)(s >> 80))!;
	stream.write_byte((char)(s >> 88))!;
	stream.write_byte((char)(s >> 96))!;
	stream.write_byte((char)(s >> 104))!;
	stream.write_byte((char)(s >> 112))!;
	stream.write_byte((char)(s >> 120))!;

}

<*
 @require @is_outstream(stream)
 @require data.len < 256 : "Data exceeded 255"
*>
macro sz? write_tiny_bytearray(stream, char[] data)
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
	char[] data = alloc::alloc_array(allocator, char, len);
	io::read_all(stream, data)!;
	return data;
}

<*
 @require @is_outstream(stream)
 @require data.len < 0x1000 : "Data exceeded 65535"
*>
macro sz? write_short_bytearray(stream, char[] data)
{
	io::write_be_ushort(stream, (ushort)data.len)!;
	return stream.write(data) + 2;
}

<*
 @require @is_instream(stream)
*>
macro char[]? read_short_bytearray(stream, Allocator allocator)
{
	int len = io::read_be_ushort(stream)!;
	if (!len) return {};
	char[] data = alloc::alloc_array(allocator, char, len);
	io::read_all(stream, data)!;
	return data;
}

<*
 @require @is_instream(stream)
*>
macro void? skip(stream, sz bytes)
{
	if (!bytes) return;
	$switch:
		$case $typeof(stream) == InStream:
			if (!&stream.seek)
			{
				for (sz i = 0; i < bytes; i++)
				{
					stream.read()!;
				}
				return;
			}
			stream.seek(bytes, FROM_CURSOR)!;
		$case $defined(stream.seek):
			stream.seek(bytes, FROM_CURSOR)!;
		$default:
			for (sz i = 0; i < bytes; i++)
			{
				stream.read()!;
			}
	$endswitch
}

<*
 Wrap bytes for reading using io functions.
*>
fn ByteReader wrap_bytes(char[] bytes)
{
	return { bytes, 0 };
}
