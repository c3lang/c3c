module std::compression::gzip_test;
import std::compression::gzip @public;
import std::io, std::math, std::compression::deflate;

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

fn void gzip_concat_nonseekable_9byte_overread() @test
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

fn void gzip_seekable_backtrack() @test
{
	@pool()
	{
		char[] m1 = gzip::compress(tmem, "A")!!;
		char[] combined = alloc::alloc_array(tmem, char, m1.len + 2);
		combined[:m1.len] = m1[..];
		combined[m1.len] = 'X';
		combined[m1.len + 1] = 'Y';

		ByteReader br = io::wrap_bytes(combined);
		ByteWriter bw; bw.tinit();

		GzipReader reader;
		reader.init(&br)!!;
		defer (void)reader.close();

		char[16] buf;
		sz n = reader.read(buf[..])!!;
		test::eq(buf[:n], "A");

		sz n_eof = reader.read(buf[..])!!;
		test::eq(n_eof, 0);

		char[2] trailing;
		sz n_trailing = br.read(trailing[..])!!;
		test::eq(n_trailing, 2);
		test::eq(trailing[..], "XY");
	};
}

fn void gzip_seekable_backtrack_1byte() @test
{
	@pool()
	{
		char[] m1 = gzip::compress(tmem, "A")!!;
		char[] combined = alloc::alloc_array(tmem, char, m1.len + 1);
		combined[:m1.len] = m1[..];
		combined[m1.len] = 'X';

		ByteReader br = io::wrap_bytes(combined);
		ByteWriter bw; bw.tinit();

		GzipReader reader;
		reader.init(&br)!!;
		defer (void)reader.close();

		char[16] buf;
		sz n = reader.read(buf[..])!!;
		test::eq(buf[:n], "A");

		sz n_eof = reader.read(buf[..])!!;
		test::eq(n_eof, 0);

		char[1] trailing;
		sz n_trailing = br.read(trailing[..])!!;
		test::eq(n_trailing, 1);
		test::eq(trailing[..], "X");
	};
}

fn void gzip_read_after_close() @test
{
	@pool()
	{
		char[] compressed = gzip::compress(tmem, "hello world")!!;
		ByteReader br = io::wrap_bytes(compressed);
		GzipReader reader;
		reader.init(&br)!!;

		char[5] buf;
		sz n = reader.read(buf[..])!!;
		test::eq(buf[:n], "hello");

		reader.close()!!;

		test::@error(reader.read(buf[..]), io::CLOSED);
	};
}

fn char[] gzip_level_data(sz len)
{
	String[*] words = { "alpha ", "beta ", "gamma ", "delta ", "epsilon ", "zeta ", "eta ", "theta " };
	char[] data = alloc::alloc_array(tmem, char, len);
	ulong x = 4242;
	sz pos = 0;
	while (pos < len)
	{
		x ^= x << 13;
		x ^= x >> 7;
		x ^= x << 17;
		foreach (c : words[(x >> 20) % words.len])
		{
			if (pos == len) break;
			data[pos++] = c;
		}
	}
	return data;
}

fn void gzip_all_levels_roundtrip_test() @test
{
	char[] data = gzip_level_data(60_000);
	for (int level = deflate::MIN_LEVEL; level <= deflate::MAX_LEVEL; level++)
	{
		char[] gz = gzip::compress(tmem, data, level: level)!!;
		char[] back = gzip::uncompress_bytes(tmem, gz)!!;
		test::eq(back, data);

		char[] gz2 = gzip::compress(tmem, data, true, "extra", "name.txt", "a comment", level)!!;
		Gzip decoded = gzip::uncompress(tmem, gz2)!!;
		test::eq(decoded.uncompressed, data);
		test::eq(decoded.filename, "name.txt");
	}
}

fn void gzip_level_sets_xfl_test() @test
{
	char[] data = gzip_level_data(5000);
	// RFC 1952 section 2.3.1: XFL is 2 for the best compression, 4 for the fastest, 0 otherwise.
	char[] fast = gzip::compress(tmem, data, level: deflate::MIN_LEVEL)!!;
	char[] best = gzip::compress(tmem, data, level: deflate::MAX_LEVEL)!!;
	char[] normal = gzip::compress(tmem, data)!!;
	assert(fast[8] == 4, "Fastest level should set XFL to 4");
	assert(best[8] == 2, "Best level should set XFL to 2");
	assert(normal[8] == 0, "Default level should leave XFL at 0");
}

fn void gzip_default_level_is_explicit_default_test() @test
{
	char[] data = gzip_level_data(40_000);
	char[] implicit = gzip::compress(tmem, data)!!;
	char[] explicit = gzip::compress(tmem, data, level: deflate::DEFAULT_LEVEL)!!;
	test::eq(implicit, explicit);
}

fn void gzip_higher_level_is_smaller_test() @test
{
	char[] data = gzip_level_data(200_000);
	char[] fast = gzip::compress(tmem, data, level: deflate::MIN_LEVEL)!!;
	char[] best = gzip::compress(tmem, data, level: deflate::MAX_LEVEL)!!;
	assert(best.len < fast.len, "The highest level must beat the lowest");
}

fn void gzip_writer_with_level_test() @test
{
	char[] data = gzip_level_data(100_000);
	foreach (int level : { 1, 6, 9 })
	{
		ByteWriter bw;
		bw.tinit();
		GzipWriter* gz = mem::tnew(GzipWriter);
		gz.init(tmem, &bw, level: level)!!;
		sz pos = 0;
		sz step = 1;
		while (pos < data.len)
		{
			sz n = math::min(step, data.len - pos);
			gz.write(data[pos:n])!!;
			pos += n;
			step = step % 2999 + 1 + step / 5;
		}
		gz.close()!!;
		char[] back = gzip::uncompress_bytes(tmem, bw.array_view())!!;
		test::eq(back, data);
	}
}
