// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
module std::compression::gzip;
import std::io, std::hash::crc32, std::compression::deflate;

faultdef CORRUPT_HEADER, CORRUPT_DATA;

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
	allocator::free(self.allocator, self.extra);
	self.filename.free(self.allocator);
	self.comment.free(self.allocator);
	allocator::free(self.allocator, self.uncompressed);
}

fn char[]? uncompress_bytes(Allocator allocator, char[] src) @inline => @pool()
{
	Gzip gz = uncompress_stream(tmem, &&io::wrap_bytes(src))!;
	return allocator::clone_slice(allocator, gz.uncompressed);
}

fn Gzip? uncompress(Allocator allocator, char[] src) @inline
{
	return uncompress_stream(allocator, &&io::wrap_bytes(src));
}

fn Gzip? uncompress_stream(Allocator allocator, InStream reader)
{
	Gzip gz = { .allocator = allocator };
	defer catch gz.free();

	@pool()
	{
		CrcWriter crc;
		crc.init();

		InStream header_reader = (TeeReader){}.init(reader, &crc);

		header_reader.read((char[GzipHeader.sizeof]*)&gz.header)!;

		if (gz.header.id != GZIP_MAGIC) return CORRUPT_HEADER~;
		if (gz.header.cm != 8) return CORRUPT_HEADER~;
		if (gz.header.flg.reserved) return CORRUPT_HEADER~;

		if (gz.header.flg.extra)
		{
			ushort xlen = io::read_le_ushort(header_reader)!;
			gz.extra = allocator::malloc(allocator, xlen)[:xlen];
			header_reader.read(gz.extra)!;
		}

		if (gz.header.flg.name)
		{
			gz.filename = decode_latin1(allocator, header_reader)!;
		}

		if (gz.header.flg.comment)
		{
			gz.comment = decode_latin1(allocator, header_reader)!;
		}

		if (gz.header.flg.hcrc)
		{
			ushort crc16 = (ushort)(crc.final() & 0xFFFF);
			ushort hcrc = io::read_le_ushort(reader)!;
			if (crc16 != hcrc) return CORRUPT_HEADER~;
		}
	};

	ByteWriter uncomp;
	uncomp.init(allocator);
	deflate::decompress_stream(reader, &uncomp)!;
	gz.uncompressed = uncomp.array_view();

	uint crc32 = io::read_le_uint(reader)!;
	if (crc32 != crc32::hash(gz.uncompressed)) return CORRUPT_DATA~;

	uint isize = io::read_le_uint(reader)!;
	if (isize != (gz.uncompressed.len & 0xFFFFFFFF)) return CORRUPT_DATA~;

	return gz;
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
	GzipHeader header = {
		.id  = GZIP_MAGIC,
		.flg = { .hcrc = hcrc, .extra = extra.len > 0, .name = name.len > 0, .comment = comment.len > 0 },
		.cm  = 8,
		.os  = 0xff,
	};

	@pool()
	{
		ByteWriter h;
		h.tinit();

		h.write((char[GzipHeader.sizeof]*)&header)!;
		if (extra.len)
		{
			io::write_le_short(&h, (ushort)extra.len)!;
			h.write(extra)!;
		}
		if (name.len)
		{
			encode_latin1(&h, name)!;
		}
		if (comment.len)
		{
			encode_latin1(&h, comment)!;
		}
		if (hcrc)
		{
			uint crc_hash = crc32::hash(h.array_view());
			io::write_le_short(&h, (ushort)(crc_hash & 0xFF_FF))!;
		}

		writer.write(h.array_view())!;

		deflate::compress_stream(tmem, &&io::wrap_bytes(src), writer)!;
	};

	io::write_le_int(writer, crc32::hash(src))!;
	io::write_le_int(writer, src.len & 0xFF_FF_FF_FF)!;
}


module std::compression::gzip @private;
import std::io, std::encoding::codepage, std::hash::crc32;

const char[2] GZIP_MAGIC  = x'1F8B';

struct CrcWriter (OutStream)
{
	Crc32 hash;
}

fn void CrcWriter.init(&self)
{
	self.hash.init();
}

fn usz? CrcWriter.write(&self, char[] bytes) @dynamic
{
	self.hash.update(bytes);
	return bytes.len;
}

fn void? CrcWriter.write_byte(&self, char c) @dynamic
{
	self.hash.updatec(c);
}

fn uint CrcWriter.final(&self) => self.hash.final();

<*
 Read a zero-terminated Latin1 string from reader.
*>
fn String? decode_latin1(Allocator allocator, InStream reader) => @pool()
{
	ByteWriter writer;
	writer.tinit();
	while (try c = reader.read_byte() && c)
	{
		writer.write_byte(c)!;
	}
	return codepage::decode(allocator, writer.array_view(), CodePage.ISO_8859_1)!;
}

<*
 Write a string s as a zero-terminated Latin1 buffer to writer.
*>
fn void? encode_latin1(OutStream writer, String s) => @pool()
{
	writer.write(codepage::encode(tmem, s, CodePage.ISO_8859_1)!)!;
	writer.write_byte('\0')!;
}
