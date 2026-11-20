<*
 Zlib compression and decompression module (RFC 1950, DEFLATE).

 A zlib stream is a two byte header, a DEFLATE stream and the Adler-32 checksum of the
 uncompressed data. Streams that require a preset dictionary (FDICT) are not supported:
 reading one returns DICTIONARY_REQUIRED, and the writer never sets FDICT.

 API:
 - fn char[]? compress(Allocator allocator, char[] src, int level = deflate::DEFAULT_LEVEL)
 - fn void?   compress_stream(OutStream writer, char[] src, int level = deflate::DEFAULT_LEVEL)
 - fn char[]? uncompress(Allocator allocator, char[] src)
 - fn void?   uncompress_stream(OutStream writer, InStream src)
 - struct ZlibWriter: OutStream
 - struct ZlibReader: InStream
*>
module std::compression::zlib;
import std::io, std::hash::adler32, std::compression::deflate, std::math;

excuse { CORRUPT_HEADER, CORRUPT_DATA, UNSUPPORTED_METHOD, DICTIONARY_REQUIRED }

// CM = 8 (DEFLATE), CINFO = 7 (32 KiB window).
const char ZLIB_CMF = 0x78;
const char ZLIB_CM_DEFLATE = 8;
const char ZLIB_FDICT = 0x20;
const usz ZLIB_READ_BUFFER = 8192;

<*
 @param level : `deflate::MIN_LEVEL (fastest) to deflate::MAX_LEVEL (smallest output).`
 @require level >= deflate::MIN_LEVEL && level <= deflate::MAX_LEVEL
*>
fn char[]? compress(Allocator allocator, char[] src, int level = deflate::DEFAULT_LEVEL)
{
	ByteWriter bw;
	bw.init(allocator);
	defer catch (void)bw.destroy();
	compress_stream(&bw, src, level)!;
	return bw.array_view();
}

<*
 @param level : `deflate::MIN_LEVEL (fastest) to deflate::MAX_LEVEL (smallest output).`
 @require level >= deflate::MIN_LEVEL && level <= deflate::MAX_LEVEL
*>
fn void? compress_stream(OutStream writer, char[] src, int level = deflate::DEFAULT_LEVEL)
{
	@pool()
	{
		ZlibWriter z;
		z.init(tmem, writer, level)!;
		z.write(src)!;
		z.close()!;
	};
}

fn char[]? uncompress(Allocator allocator, char[] src)
{
	ByteReader br = io::wrap_bytes(src);
	ByteWriter bw;
	bw.init(allocator);
	defer catch (void)bw.destroy();
	uncompress_stream(&bw, &br)!;
	return bw.array_view();
}

fn void? uncompress_stream(OutStream writer, InStream src)
{
	@pool()
	{
		ZlibReader reader;
		reader.init(src, tmem)!;
		defer (void)reader.close();
		char[32768] buf;
		while (sz n = io::read_buffer(&reader, &buf)!)
		{
			writer.write(buf[:n])!;
		}
	};
}

struct ZlibWriter (OutStream)
{
	OutStream next;
	Deflater deflater;
	Adler32 hasher;
}

<*
 Writes the zlib header and prepares the compressor. Call `close` to finish the stream.

 @param level : `deflate::MIN_LEVEL (fastest) to deflate::MAX_LEVEL (smallest output).`
 @require level >= deflate::MIN_LEVEL && level <= deflate::MAX_LEVEL
*>
fn void? ZlibWriter.init(&self, Allocator allocator, OutStream writer, int level = deflate::DEFAULT_LEVEL)
{
	self.next = writer;
	self.hasher.init();

	// FLEVEL is only a hint: 0 fastest, 1 fast, 2 default, 3 slowest.
	char flevel = level == deflate::MIN_LEVEL ? 0 : (level < deflate::DEFAULT_LEVEL ? 1 : (level == deflate::DEFAULT_LEVEL ? 2 : 3));
	char flg = (char)(flevel << 6);
	// FCHECK makes the 16 bit header a multiple of 31.
	flg += (char)(31 - ((uint)ZLIB_CMF * 256 + flg) % 31) % 31;
	char[2] header = { ZLIB_CMF, flg };
	self.next.write(&header)!;

	self.deflater.init(allocator, self.next, level);
}

fn sz? ZlibWriter.write(&self, char[] bytes) @dynamic
{
	if (!bytes.len) return 0;
	self.hasher.update(bytes);
	return self.deflater.write(bytes);
}

fn void? ZlibWriter.write_byte(&self, char c) @dynamic
{
	self.hasher.update_byte(c);
	return self.deflater.write_byte(c);
}

fn void? ZlibWriter.close(&self) @dynamic
{
	self.deflater.close()!;
	io::write_be_uint(self.next, self.hasher.final())!;
}

struct ZlibReader (InStream)
{
	InStream source;
	Inflater* inflater;
	Adler32 hasher;
	char* bit_buf;
	Allocator allocator;
	bool header_read;
	bool finished;
}

fn void? ZlibReader.init(&self, InStream reader, Allocator allocator = tmem)
{
	*self = { .source = reader, .allocator = allocator };
	self.hasher.init();
}

fn sz? ZlibReader.read(&self, char[] bytes) @dynamic
{
	if (!self.source) return io::CLOSED~;
	if (self.finished || !bytes.len) return 0;

	if (!self.header_read)
	{
		self.read_header()!;
		self.header_read = true;

		self.bit_buf = alloc::alloc_array(self.allocator, char, ZLIB_READ_BUFFER);
		self.inflater = alloc::new(self.allocator, deflate::Inflater);
		self.inflater.init(self.source, self.bit_buf[:ZLIB_READ_BUFFER]);
	}

	sz n = io::read_buffer(self.inflater, bytes)!;
	if (n > 0)
	{
		self.hasher.update(bytes[:n]);
		return n;
	}

	// End of the DEFLATE stream: the Adler-32 follows. A seekable source is rewound by the inflater,
	// otherwise the bytes it read ahead are taken from its buffer.
	sz unused = !&self.source.seek || @catch(self.source.seek(0, FROM_CURSOR))
		? self.inflater.copy_unused_input(self.bit_buf[:ZLIB_READ_BUFFER]) : 0;
	self.inflater.close()!;

	char[4] footer;
	sz footer_len = (sz)footer.len;
	sz have = math::min(unused, footer_len);
	mem::copy(&footer, self.bit_buf, (usz)have);
	if (have < footer_len)
	{
		if (read_full(self.source, footer[have..])! != footer_len - have) return CORRUPT_DATA~;
	}
	uint stored = (uint)footer[0] << 24 | (uint)footer[1] << 16 | (uint)footer[2] << 8 | footer[3];
	if (stored != self.hasher.final()) return CORRUPT_DATA~;

	// Hand back what follows the stream when the source can seek.
	if (unused > footer_len && &self.source.seek)
	{
		(void)self.source.seek(-(long)(unused - footer_len), FROM_CURSOR);
	}

	self.finished = true;
	alloc::free(self.allocator, self.inflater);
	self.inflater = null;
	return 0;
}

fn void? ZlibReader.read_header(&self)
{
	char[2] h;
	if (read_full(self.source, &h)! != 2) return CORRUPT_HEADER~;
	if (((uint)h[0] * 256 + h[1]) % 31 != 0) return CORRUPT_HEADER~;
	if ((h[0] & 0x0F) != ZLIB_CM_DEFLATE) return UNSUPPORTED_METHOD~;
	if ((h[0] >> 4) > 7) return CORRUPT_HEADER~;
	if (h[1] & ZLIB_FDICT) return DICTIONARY_REQUIRED~;
}

fn void? ZlibReader.close(&self) @dynamic
{
	if (self.inflater)
	{
		alloc::free(self.allocator, self.inflater);
		self.inflater = null;
	}
	if (self.bit_buf)
	{
		alloc::free(self.allocator, self.bit_buf);
		self.bit_buf = null;
	}
	self.source = {};
}

fn char? ZlibReader.read_byte(&self) @dynamic
{
	return io::read_byte_using_read(self);
}

<*
 Reads until `buf` is full or the stream ends, since a single read may return less.
*>
fn sz? read_full(InStream stream, char[] buf) @private
{
	sz total = 0;
	sz wanted = (sz)buf.len;
	while (total < wanted)
	{
		sz? n = io::read_buffer(stream, buf[total..]);
		if (catch err = n)
		{
			if (err == io::EOF) break;
			return err~;
		}
		if (!n) break;
		total += n;
	}
	return total;
}
