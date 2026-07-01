module std::compression::gzip_test;
import std::compression::gzip @public;
import std::io, std::math;

fn void gzip_fhcrc() @test
{
	char[*] data =
	x"1F8B080200000000020B179D010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg.hcrc);
}

fn void gzip_fextra() @test
{
	char[*] data =
	x"1F8B080400000000020B040064617461010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg.extra);
	assert(gz.extra.len);
	test::eq(gz.extra, "data");
}

fn void gzip_fname() @test
{
	char[*] data =
	x"1F8B080800000000020B666F6F2E6300010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg.name);
	assert(gz.filename.len);
	test::eq(gz.filename, "foo.c");
}

fn void gzip_fcomment() @test
{
	char[*] data =
	x"1F8B081000000000020B68656C6C6F00010100FEFF008DEF02D201000000";

	Gzip gz = gzip::uncompress(mem, &data)!!;
	defer gz.free();

	assert(gz.header.flg.comment);
	assert(gz.comment.len);
	test::eq(gz.comment, "hello");
}

fn void gzip_round_trip() @test
{
	char[] extra    = "extra data";
	String filename = "filename_éä.c";
	String comment  = "comment\nline";
	char[] data     = "foo bar";

	char[] gz = gzip::compress(mem, data, hcrc: true,
	extra: extra, name: filename, comment: comment)!!;
	defer alloc::free(mem, gz.ptr);

	Gzip decode = gzip::uncompress(mem, gz)!!;
	defer decode.free();

	test::eq(decode.extra, extra);
	test::eq(decode.filename, filename);
	test::eq(decode.comment, comment);
	test::eq(decode.uncompressed, data);
}

fn void gzip_empty_data() @test
{
	char[] data = "";
	char[] compressed = gzip::compress(mem, data)!!;
	defer alloc::free(mem, compressed.ptr);

	char[] uncompressed = gzip::uncompress_bytes(mem, compressed)!!;
	defer alloc::free(mem, uncompressed.ptr);

	test::eq(uncompressed.len, 0);
}

fn void gzip_streaming_writer() @test
{
	char[] data = "streaming test data";
	ByteWriter bw;
	bw.tinit();

	GzipWriter gz;
	gz.init(tmem, &bw, name: "test.txt")!!;
	gz.write(data)!!;
	gz.close()!!;

	char[] compressed = bw.array_view();
	Gzip decode = gzip::uncompress(tmem, compressed)!!;
	test::eq(decode.uncompressed, data);
	test::eq(decode.filename, "test.txt");
}

fn void gzip_streaming_reader() @test
{
	char[] data = "inflater streaming test";
	char[] compressed = gzip::compress(tmem, data)!!;

	ByteReader br = io::wrap_bytes(compressed);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	ByteWriter bw;
	bw.tinit();

	char[4] chunk; // Small chunks to test streaming boundary
	while (true)
	{
		sz n = reader.read(chunk[..])!!;
		if (n == 0) break;
		bw.write(chunk[:n])!!;
	}

	test::eq(bw.array_view(), data);
}

fn void gzip_streaming_round_trip() @test
{
	// Large'ish data to verify buffer boundaries
	sz size = 65536;
	char[] data = alloc::alloc_array(tmem, char, size);
	for (sz i = 0; i < size; i++) data[i] = (char)(i & 0xFF);

	ByteWriter bw;
	bw.tinit();

	GzipWriter writer;
	writer.init(tmem, &bw)!!;
	writer.write(data)!!;
	writer.close()!!;

	ByteReader br = io::wrap_bytes(bw.array_view());
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	ByteWriter result;
	result.tinit();

	char[4096] buf;
	while (true)
	{
		sz n = reader.read(buf[..])!!;
		if (n == 0) break;
		result.write(buf[:n])!!;
	}

	test::eq(result.array_view(), data);
}

fn void gzip_corrupt_magic() @test
{
	char[] data = "foo";
	char[] compressed = gzip::compress(tmem, data)!!;
	compressed[0] = 0; // Corrupt magic

	ByteReader br = io::wrap_bytes(compressed);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	char[10] buf;
	sz? res = reader.read(buf[..]);
	if (catch err = res)
	{
		test::eq(err, gzip::CORRUPT_HEADER);
		return;
	}
	assert(false, "Expected CORRUPT_HEADER fault");
}

fn void gzip_corrupt_crc() @test
{
	char[] data = "hello world";
	char[] compressed = gzip::compress(tmem, data)!!;
	compressed[compressed.len - 8]++; // Corrupt CRC

	ByteReader br = io::wrap_bytes(compressed);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	char[32] buf;
	while (true)
	{
		sz? n = reader.read(buf[..]);
		if (catch err = n)
		{
			test::eq(err, gzip::CORRUPT_DATA);
			return;
		}
		if (n == 0) break;
	}
	assert(false, "Expected CORRUPT_DATA fault");
}

fn void gzip_corrupt_isize() @test
{
	char[] data = "hello world";
	char[] compressed = gzip::compress(tmem, data)!!;
	compressed[compressed.len - 1]++; // Corrupt ISIZE

	ByteReader br = io::wrap_bytes(compressed);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	char[32] buf;
	while (true)
	{
		sz? n = reader.read(buf[..]);
		if (catch err = n)
		{
			test::eq(err, gzip::CORRUPT_DATA);
			return;
		}
		if (n == 0) break;
	}
	assert(false, "Expected CORRUPT_DATA fault");
}

fn void gzip_unexpected_eof() @test
{
	char[] data = "unexpected eof test content";
	char[] compressed = gzip::compress(tmem, data)!!;
	char[] truncated = compressed[..compressed.len - 2]; // Truncate footer

	ByteReader br = io::wrap_bytes(truncated);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	char[64] buf;
	while (true)
	{
		sz? n = reader.read(buf[..]);
		if (catch err = n)
		{
			test::@check(err == io::EOF || err == gzip::CORRUPT_DATA, "unexpected error: %s", err);
			return;
		}
		if (n == 0) break;
	}
	assert(false, "Expected EOF fault");
}
fn void gzip_compress_stream_test() @test
{
	char[] data = "test data for compress_stream";
	ByteWriter bw;
	bw.tinit();

	gzip::compress_stream(&bw, data)!!;

	Gzip decode = gzip::uncompress(tmem, bw.array_view())!!;
	test::eq(decode.uncompressed, data);
}

fn void gzip_uncompress_stream_test() @test
{
	char[] data = "test data for uncompress_stream";
	char[] compressed = gzip::compress(tmem, data)!!;

	ByteReader br = io::wrap_bytes(compressed);
	ByteWriter bw;
	bw.tinit();

	gzip::uncompress_stream(&bw, &br)!!;

	test::eq(bw.array_view(), data);
}

fn void gzip_concatenated() @test
{
	char[] data1 = "first member ";
	char[] data2 = "second member";
	char[] gz1 = gzip::compress(tmem, data1)!!;
	char[] gz2 = gzip::compress(tmem, data2)!!;

	// Concatenate, it kinda magical that it works (dead emoji)
	char[] combined = alloc::alloc_array(tmem, char, gz1.len + gz2.len);
	combined[:gz1.len] = gz1[..];
	combined[gz1.len..] = gz2[..];

	ByteReader br = io::wrap_bytes(combined);
	ByteWriter bw;
	bw.tinit();
	gzip::uncompress_stream(&bw, &br)!!;

	test::eq(bw.array_view(), "first member second member");
}

fn void gzip_truncated_header() @test
{
	char[] data = "foo";
	char[] compressed = gzip::compress(tmem, data)!!;
	char[] truncated = compressed[..5]; // Truncate early in header

	ByteReader br = io::wrap_bytes(truncated);
	GzipReader reader;
	reader.init(&br)!!;
	defer (void)reader.close();

	char[10] buf;
	sz? res = reader.read(buf[..]);
	if (catch err = res)
	{
		test::eq(err, gzip::IO_ERROR);
		return;
	}
	assert(false, "expected IO_ERROR for truncated header");
}

struct PipeReader (InStream) @local
{
	char[] data;
	usz pos;
}

// Delivers up to split_at bytes in one read, then 1 byte at a time.
// Always returns UNSUPPORTED_OPERATION for seek (simulates a pipe).
struct SplitAtReader (InStream) @local
{
	char[] data;
	sz pos;
	sz split_at;
}

fn sz? PipeReader.read(&self, char[] buffer) @dynamic
{
	if (self.pos >= self.data.len) return 0;
	usz remaining = self.data.len - self.pos;
	usz n = buffer.len < remaining ? buffer.len : remaining;
	buffer[:n] = self.data[self.pos:n];
	self.pos += n;
	return (sz)n;
}

fn char? PipeReader.read_byte(&self) @dynamic
{
	if (self.pos >= self.data.len) return io::EOF~;
	return self.data[self.pos++];
}

fn void? PipeReader.seek(&self, long offset, SeekOrigin origin = FROM_START) @dynamic
{
	return io::UNSUPPORTED_OPERATION~;
}

fn sz? SplitAtReader.read(&self, char[] buffer) @dynamic
{
	if (self.pos >= self.data.len) return 0;
	sz available = self.data.len - self.pos;
	sz limit = self.pos < self.split_at ? self.split_at - self.pos : 1;
	sz n = math::min(buffer.len, math::min(limit, available));
	buffer[:n] = self.data[self.pos:n];
	self.pos += n;
	return (sz)n;
}

fn char? SplitAtReader.read_byte(&self) @dynamic
{
	if (self.pos >= self.data.len) return io::EOF~;
	return self.data[self.pos++];
}

fn void? SplitAtReader.seek(&self, long offset, SeekOrigin origin = FROM_START) @dynamic
{
	return io::UNSUPPORTED_OPERATION~;
}

fn void gzip_corrupt_footer_seekable() @test
{
	@pool()
	{
		DString ds; ds.tinit();
		for (int i = 0; i < 20; i++) ds.append("The quick brown fox jumps over the lazy dog. ");
		char[] gz = gzip::compress(tmem, (char[])ds.str_view())!!;
		gz[^8] ^= 0xFF;
		test::@error(gzip::uncompress_bytes(tmem, gz), gzip::CORRUPT_DATA);
	};
}

fn void gzip_corrupt_footer_non_seekable() @test
{
	@pool()
	{
		DString ds;
		for (int i = 0; i < 20; i++) ds.append("The quick brown fox jumps over the lazy dog. ");
		char[] gz = gzip::compress(tmem, (char[])ds.str_view())!!;
		gz[^8] ^= 0xFF;

		PipeReader pr = { .data = gz };
		ByteWriter bw; bw.tinit();
		test::@error(gzip::uncompress_stream(&bw, &pr), gzip::CORRUPT_DATA);
	};
}

fn void gzip_concat_nonseekable_9byte_overread() @test @if(false)
{
	@pool()
	{
		char[] m1 = gzip::compress(tmem, "A")!!;
		char[] m2 = gzip::compress(tmem, "B")!!;

		char[] combined = alloc::alloc_array(tmem, char, m1.len + m2.len);
		combined[:m1.len] = m1[..];
		combined[m1.len..] = m2[..];

		// m1 = [header=10][deflate=N][footer=8]
		// split_at = 10 + N + 9 forces the BitReader's first fill to capture
		// the full deflate block plus exactly 9 extra bytes (footer[0..7] + m2[0]).
		sz deflate_len = m1.len - 18;
		SplitAtReader sr = { .data = combined, .split_at = 10 + deflate_len + 9 };

		ByteWriter bw; bw.tinit();
		gzip::uncompress_stream(&bw, &sr)!!;
		test::eq(bw.array_view(), "AB");
	};
}
