<*
 Gzip compression and decompression module (DEFLATE).

 API:
 - fn char[]?   compress(Allocator allocator, char[] src, ...)
 - fn void?     compress_stream(OutStream writer, char[] src, ...)
 - fn Gzip?     uncompress(Allocator allocator, char[] src)
 - fn char[]?   uncompress_bytes(Allocator allocator, char[] src)
 - fn void?     uncompress_stream(OutStream writer, InStream src)
 - struct GzipWriter
 - struct GzipReader

 Initial implementation by Koni Marti.
 Streaming and concatenation support added by Manu Linares.
*>
module std::compression::gzip;
import std::io, std::hash::crc32, std::compression::deflate, std::encoding::codepage;

faultdef CORRUPT_HEADER, CORRUPT_DATA, UNSUPPORTED_METHOD, IO_ERROR;

struct GzipHeader @packed
{
	char[2] id;
	char cm;
	bitstruct flg : char
	{
		bool text     : 0;
		bool hcrc     : 1;
		bool extra    : 2;
		bool name     : 3;
		bool comment  : 4;
		uint reserved : 5..7;
	}
	UIntLE mtime;
	char xfl;
	char os;
}

const char GZIP_ID1 = 0x1F;
const char GZIP_ID2 = 0x8B;
const char GZIP_CM_DEFLATE = 8;

struct Gzip
{
	Allocator allocator;
	GzipHeader header;
	char[] extra;
	String filename;
	String comment;
	char[] uncompressed;
}

fn void Gzip.free(&self)
{
	if (!self.allocator) return;
	alloc::free(self.allocator, self.extra);
	self.filename.free(self.allocator);
	self.comment.free(self.allocator);
	alloc::free(self.allocator, self.uncompressed);
}

fn char[]? compress(Allocator allocator, char[] src, bool hcrc = false,
	char[] extra = {}, String name = "", String comment = "")
{
	ByteWriter bw;
	bw.init(allocator);
	compress_stream(&bw, src, hcrc, extra, name, comment)!;
	return bw.array_view();
}

fn void? compress_stream(OutStream writer, char[] src, bool hcrc = false, char[] extra = {}, String name = "", String comment = "")
{
	GzipWriter gz;
	gz.init(tmem, writer, hcrc, extra, name, comment)!;
	gz.write(src)!;
	gz.close()!;
}

fn Gzip? uncompress(Allocator allocator, char[] src)
{
	ByteReader br = io::wrap_bytes(src);
	GzipReader reader;
	reader.init(&br, allocator)!;
	defer (void)reader.close();

	ByteWriter bw;
	bw.tinit();

	do_uncompress(&reader, &bw)!;

	Gzip gz = {
		.allocator = allocator,
		.header = reader.metadata.header,
		.extra = alloc::clone_slice(allocator, reader.metadata.extra),
		.filename = reader.metadata.filename.copy(allocator),
		.comment = reader.metadata.comment.copy(allocator),
		.uncompressed = alloc::clone_slice(allocator, bw.array_view())
	};
	return gz;
}

fn char[]? uncompress_bytes(Allocator allocator, char[] src)
{
	ByteReader br = io::wrap_bytes(src);
	ByteWriter bw;
	bw.init(allocator);
	uncompress_stream(&bw, &br)!;
	return bw.array_view();
}

fn void? uncompress_stream(OutStream writer, InStream src)
{
	GzipReader reader;
	reader.init(src, tmem)!;
	defer (void)reader.close();
	do_uncompress(&reader, writer)!;
}

fn void? do_uncompress(GzipReader* reader, OutStream writer) @private
{
	char[32768] buf;
	while (true)
	{
		sz n = reader.read(buf[..])!;
		if (n == 0) break;
		writer.write(buf[:n])!;
	}
}


struct GzipWriter (OutStream)
{
	OutStream next;
	deflate::Deflater deflater;
	crc32::Crc32 hasher;
	usz total_in;
}

fn void? GzipWriter.init(&self, Allocator allocator, OutStream writer, bool hcrc = false,
	char[] extra = {}, String name = "", String comment = "")
{
	self.next = writer;
	self.hasher.init();
	self.total_in = 0;

	GzipHeader header = {
		.id  = { GZIP_ID1, GZIP_ID2 },
		.flg = { .hcrc = hcrc, .extra = extra.len > 0, .name = name.len > 0, .comment = comment.len > 0 },
		.cm  = GZIP_CM_DEFLATE,
		.os  = 0xff,
	};

	ByteWriter h;
	h.tinit();

	h.write(((char*)&header)[:GzipHeader::size])!;
	if (extra.len)
	{
		io::write_le_ushort(&h, (ushort)extra.len)!;
		h.write(extra)!;
	}
	if (name.len) encode_latin1(&h, name)!;
	if (comment.len) encode_latin1(&h, comment)!;
	if (hcrc)
	{
		uint crc_hash = crc32::hash(h.array_view());
		io::write_le_ushort(&h, (ushort)(crc_hash & 0xFF_FF))!;
	}
	self.next.write(h.array_view())!;

	self.deflater.init(allocator, self.next);
}

fn sz? GzipWriter.write(&self, char[] bytes) @dynamic
{
	if (!bytes.len) return 0;
	self.hasher.update(bytes);
	self.total_in += (usz)bytes.len;
	return self.deflater.write(bytes);
}

fn void? GzipWriter.write_byte(&self, char c) @dynamic
{
	self.hasher.update_byte(c);
	self.total_in++;
	return self.deflater.write_byte(c);
}

fn void? GzipWriter.close(&self) @dynamic
{
	self.deflater.close()!;
	io::write_le_uint(self.next, self.hasher.final())!;
	io::write_le_uint(self.next, (uint)(self.total_in & 0xFFFFFFFF))!;
}

struct GzipMetadata
{
	GzipHeader header;
	char[] extra;
	String filename;
	String comment;
}

struct GzipReader (InStream)
{
	InStream source;
	deflate::Inflater* inflater;
	crc32::Crc32 hasher;
	usz total_out;
	bool header_read;
	bool footer_verified;
	GzipMetadata metadata;
	char* bit_buf;
	Allocator allocator;
}

fn void? GzipReader.init(&self, InStream reader, Allocator allocator = tmem)
{
	self.source = reader;
	self.allocator = allocator;
	self.hasher.init();
	self.total_out = 0;
	self.header_read = false;
	self.footer_verified = false;
	self.metadata = {};
	self.inflater = null;
}

fn void GzipReader.free_metadata(&self, Allocator allocator)
{
	if (self.metadata.extra.ptr) alloc::free(allocator, self.metadata.extra);
	if (self.metadata.filename.len > 0) self.metadata.filename.free(allocator);
	if (self.metadata.comment.len > 0) self.metadata.comment.free(allocator);
	self.metadata.extra = {};
	self.metadata.filename = "";
	self.metadata.comment = "";
}

fn sz? GzipReader.read(&self, char[] bytes) @dynamic
{
	if (self.footer_verified) return 0;

	if (!self.header_read)
	{
		self.read_header()!;
		self.header_read = true;

		self.bit_buf = alloc::alloc_array(self.allocator, char, 8192);
		self.inflater = alloc::new(self.allocator, deflate::Inflater);
		self.inflater.init(self.source, self.bit_buf[:8192]);
	}

	sz? n_res = self.inflater.read(bytes);
	if (catch err = n_res)
	{
		return err~;
	}

	sz n = n_res;
	if (n > 0)
	{
		self.hasher.update(bytes[:n]);
		self.total_out += (usz)n;
		return n;
	}

	if (!self.footer_verified)
	{
		self.inflater.close()!;

		if (try self.source.seek(0, FROM_CURSOR))
		{
			uint crc_stored = io::read_le_uint(self.source)!;
			uint isize_stored = io::read_le_uint(self.source)!;

			if (crc_stored != self.hasher.final()) return CORRUPT_DATA~;
			if (isize_stored != (uint)(self.total_out & uint::max)) return CORRUPT_DATA~;
		}

		self.footer_verified = true;

		alloc::free(self.allocator, self.bit_buf);
		alloc::free(self.allocator, self.inflater);
		self.inflater = null;
		self.bit_buf = null;
	}

	char[2] magic;
	sz? next_magic = self.source.read(magic[..]);
	if (try nm = next_magic && nm == 2)
	{
		if (magic[0] == GZIP_ID1 && magic[1] == GZIP_ID2)
		{
			self.header_read = false;
			self.footer_verified = false;
			self.hasher.init();
			self.total_out = 0;
			self.free_metadata(self.allocator);

			self.source.seek(-2, FROM_CURSOR)!;
			return self.read(bytes);
		}
		self.source.seek(-2, FROM_CURSOR)!;
	}

	return 0;
}

fn void? GzipReader.read_header(&self) @dynamic
{
	ByteWriter h;
	h.tinit();

	char[GzipHeader::size] h_bytes;
	sz n = self.source.read(h_bytes[..])!;
	if (n != GzipHeader::size) return IO_ERROR~;
	h.write(h_bytes[..])!;

	self.metadata.header = *(GzipHeader*)&h_bytes;

	if (self.metadata.header.id[0] != GZIP_ID1 || self.metadata.header.id[1] != GZIP_ID2) return CORRUPT_HEADER~;
	if (self.metadata.header.cm != GZIP_CM_DEFLATE) return UNSUPPORTED_METHOD~;
	if (self.metadata.header.flg.reserved) return CORRUPT_HEADER~;

	if (self.metadata.header.flg.extra)
	{
		ushort xlen = io::read_le_ushort(self.source)!;
		io::write_le_ushort(&h, xlen)!;

		self.metadata.extra = alloc::alloc_array(self.allocator, char, xlen);
		self.source.read(self.metadata.extra)!;
		h.write(self.metadata.extra)!;
	}

	if (self.metadata.header.flg.name)
	{
		ByteWriter name_bw;
		name_bw.tinit();
		while (true)
		{
			char c = self.source.read_byte()!;
			h.write_byte(c)!;
			if (c == '\0') break;
			name_bw.write_byte(c)!;
		}
		self.metadata.filename = codepage::decode(self.allocator, name_bw.array_view(), CodePage.ISO_8859_1)!;
	}

	if (self.metadata.header.flg.comment)
	{
		ByteWriter comment_bw;
		comment_bw.tinit();
		while (true)
		{
			char c = self.source.read_byte()!;
			h.write_byte(c)!;
			if (c == '\0') break;
			comment_bw.write_byte(c)!;
		}
		self.metadata.comment = codepage::decode(self.allocator, comment_bw.array_view(), CodePage.ISO_8859_1)!;
	}

	if (self.metadata.header.flg.hcrc)
	{
		uint crc_hash = crc32::hash(h.array_view());
		ushort crc16 = (ushort)(crc_hash & 0xFFFF);
		ushort hcrc = io::read_le_ushort(self.source)!;
		if (crc16 != hcrc) return CORRUPT_HEADER~;
	}
}

fn void? GzipReader.close(&self) @dynamic
{
	self.free_metadata(self.allocator);
	if (self.inflater)
	{
		alloc::free(self.allocator, self.bit_buf);
		alloc::free(self.allocator, self.inflater);
		self.inflater = null;
	}
	return;
}

fn char? GzipReader.read_byte(&self) @dynamic
{
	char[1] b;
	sz n = self.read(b[..])!;
	if (n == 0) return io::EOF~;
	return b[0];
}

fn void? encode_latin1(OutStream writer, String s)
{
	@pool() {
		writer.write(codepage::encode(tmem, s, CodePage.ISO_8859_1)!)!;
		writer.write_byte('\0')!;
	};
}
