module std::io @test;
import std::compression::gzip, std::compression::deflate;

struct ChunkedReader (InStream)
{
	char[] bytes;
	sz index;
	sz chunk;
}

fn ChunkedReader* ChunkedReader.init(&self, char[] bytes, sz chunk)
{
	*self = { .bytes = bytes, .chunk = chunk };
	return self;
}

fn sz? ChunkedReader.read(&self, char[] buffer) @dynamic
{
	if (self.index >= self.bytes.len) return io::EOF~;
	sz len = min(min(self.bytes.len - self.index, buffer.len), self.chunk);
	if (!len) return 0;
	mem::copy(buffer.ptr, &self.bytes[self.index], len);
	self.index += len;
	return len;
}

fn char? ChunkedReader.read_byte(&self) @dynamic
{
	if (self.index >= self.bytes.len) return io::EOF~;
	return self.bytes[self.index++];
}

struct ZeroEndReader (InStream)
{
	char[] bytes;
	sz index;
	sz chunk;
}

fn ZeroEndReader* ZeroEndReader.init(&self, char[] bytes, sz chunk)
{
	*self = { .bytes = bytes, .chunk = chunk };
	return self;
}

fn sz? ZeroEndReader.read(&self, char[] buffer) @dynamic
{
	sz left = self.bytes.len - self.index;
	sz len = min(min(left, buffer.len), self.chunk);
	if (!len) return 0;
	mem::copy(buffer.ptr, &self.bytes[self.index], len);
	self.index += len;
	return len;
}

fn char? ZeroEndReader.read_byte(&self) @dynamic
{
	if (self.index >= self.bytes.len) return 0;
	return self.bytes[self.index++];
}

const String SAMPLE = "0123456789abcdefghijklmnopqrstuvwxyz";

fn void read_all_survives_partial_reads()
{
	for (sz chunk = 1; chunk <= 8; chunk++)
	{
		ChunkedReader r;
		r.init((char[])SAMPLE, chunk);
		char[36] buf;
		sz n = io::read_all(&r, &buf)!!;
		assert(n == SAMPLE.len, "chunk %d: got %d", chunk, n);
		assert((String)buf[..] == SAMPLE, "chunk %d: wrong data", chunk);
	}
}

fn void read_all_still_detects_real_truncation()
{
	ChunkedReader r;
	r.init((char[])SAMPLE[:5], 2);
	char[36] buf;
	test::@error(io::read_all(&r, &buf), io::UNEXPECTED_EOF);
}

fn void read_all_zero_length_buffer()
{
	ChunkedReader r;
	r.init((char[])SAMPLE, 4);
	char[4] buf;
	assert(io::read_all(&r, buf[0:0])!! == 0);
}

fn void read_any_survives_partial_reads()
{
	// An 8-byte struct delivered 3 bytes at a time.
	ChunkedReader r;
	r.init((char[])SAMPLE, 3);
	int[2] pair;
	io::read_any(&r, &pair)!!;
	assert(((char*)&pair)[:8] == (char[])SAMPLE[:8]);
}

fn void read_buffer_to_end_fills_across_chunks()
{
	ChunkedReader r;
	r.init((char[])SAMPLE, 5);
	char[36] buf;
	char[] got = io::read_buffer_to_end(&r, &buf)!!;
	assert(got.len == SAMPLE.len, "got %d", got.len);
	assert((String)got == SAMPLE);
}

fn void read_buffer_to_end_partial_on_short_stream()
{
	ChunkedReader r;
	r.init((char[])SAMPLE[:10], 3);
	char[36] buf;
	char[] got = io::read_buffer_to_end(&r, &buf)!!;
	assert(got.len == 10, "got %d", got.len);
}

fn void read_buffer_normalizes_both_end_conventions()
{
	ChunkedReader eof_style;
	eof_style.init((char[])SAMPLE[:2], 4);
	char[8] buf;
	assert(io::read_buffer(&eof_style, &buf)!! == 2);
	assert(io::read_buffer(&eof_style, &buf)!! == 0, "EOF fault must become 0");

	ZeroEndReader zero_style;
	zero_style.init((char[])SAMPLE[:2], 4);
	assert(io::read_buffer(&zero_style, &buf)!! == 2);
	assert(io::read_buffer(&zero_style, &buf)!! == 0);
}

fn void copy_to_handles_partial_reads()
{
	foreach (chunk : (sz[]){ 1, 3, 7, 100 })
	{
		ChunkedReader r;
		r.init((char[])SAMPLE, chunk);
		ByteWriter w;
		io::copy_to(&r, w.tinit())!!;
		assert(w.str_view() == SAMPLE, "chunk %d: got '%s'", chunk, w.str_view());
	}
}

fn void copy_to_handles_zero_end_convention()
{
	ZeroEndReader r;
	r.init((char[])SAMPLE, 3);
	ByteWriter w;
	io::copy_to(&r, w.tinit())!!;
	assert(w.str_view() == SAMPLE);
}

macro void @assert_drains(stream, String expected, String name)
{
	ByteWriter w;
	io::copy_to(stream, w.tinit())!!;
	assert(w.str_view() == expected, "%s: got '%s', want '%s'", name, w.str_view(), expected);
}

fn void bytereader_conformance()
{
	ByteReader r;
	r.init((char[])SAMPLE);
	@assert_drains(&r, SAMPLE, "ByteReader");
	// Past the end keeps reporting the end.
	char[4] buf;
	assert(io::read_buffer(&r, &buf)!! == 0);
	assert(io::read_buffer(&r, &buf)!! == 0);
}

fn void bytebuffer_conformance()
{
	ByteBuffer bb;
	bb.tinit(64);
	bb.write((char[])SAMPLE)!!;
	@assert_drains(&bb, SAMPLE, "ByteBuffer");
}

fn void limitreader_conformance()
{
	ChunkedReader src;
	src.init((char[])SAMPLE, 3);
	LimitReader lr;
	lr.init(&src, 10);
	@assert_drains(&lr, SAMPLE[:10], "LimitReader");
}

fn void limitreader_stops_at_limit_not_chunk()
{
	// The limit must be honoured exactly even when the source hands out
	// fewer bytes per read than the limit.
	ChunkedReader src;
	src.init((char[])SAMPLE, 1);
	LimitReader lr;
	lr.init(&src, 5);
	char[36] buf;
	char[] got = io::read_buffer_to_end(&lr, &buf)!!;
	assert(got.len == 5, "got %d", got.len);
	assert((String)got == SAMPLE[:5]);
}

fn void multireader_conformance()
{
	ByteReader a, b, c;
	a.init((char[])SAMPLE[:10]);
	b.init((char[])SAMPLE[10:10]);
	c.init((char[])SAMPLE[20..]);
	MultiReader mr;
	mr.tinit(&a, &b, &c);
	@assert_drains(&mr, SAMPLE, "MultiReader");
}

fn void multireader_over_chunked_sources()
{
	ChunkedReader a, b;
	a.init((char[])SAMPLE[:18], 2);
	b.init((char[])SAMPLE[18..], 3);
	MultiReader mr;
	mr.tinit(&a, &b);
	@assert_drains(&mr, SAMPLE, "MultiReader/chunked");
}

fn void multireader_skips_empty_and_repeats_eof()
{
	ByteReader e1, x, e2, y;
	e1.init((char[])""); x.init((char[])"X");
	e2.init((char[])""); y.init((char[])"Y");
	MultiReader mr;
	mr.tinit(&e1, &x, &e2, &y);
	@assert_drains(&mr, "XY", "MultiReader/empty");
	// Reading past the end must stay at EOF, not run off the reader array.
	char[4] buf;
	test::eq(mr.read(&buf)!!, 0);
	test::eq(mr.read(&buf)!!, 0);
	test::@error(mr.read_byte(), io::EOF);
}

fn void teereader_conformance()
{
	ChunkedReader src;
	src.init((char[])SAMPLE, 4);
	ByteWriter side;
	side.tinit();
	TeeReader tr;
	tr.init(&src, &side);
	@assert_drains(&tr, SAMPLE, "TeeReader");
	assert(side.str_view() == SAMPLE, "tee side got '%s'", side.str_view());
}

fn void readbuffer_conformance()
{
	ChunkedReader src;
	src.init((char[])SAMPLE, 3);
	ReadBuffer rb;
	rb.init(&src, mem::temp_array(char, 8));
	@assert_drains(&rb, SAMPLE, "ReadBuffer");
}

fn void readbuffer_large_request_bypass()
{
	// A request larger than the internal buffer takes the direct path.
	ChunkedReader src;
	src.init((char[])SAMPLE, 5);
	ReadBuffer rb;
	rb.init(&src, mem::temp_array(char, 4));
	char[36] buf;
	char[] got = io::read_buffer_to_end(&rb, &buf)!!;
	assert(got.len == SAMPLE.len, "got %d", got.len);
	assert((String)got == SAMPLE);
}

fn void scanner_conformance()
{
	ChunkedReader src;
	src.init((char[])"alpha\nbeta\ngamma", 2);
	Scanner sc;
	sc.init(&src, mem::temp_array(char, 32));
	assert((String)sc.scan("\n")!! == "alpha");
	assert((String)sc.scan("\n")!! == "beta");
}

fn void inflater_over_chunked_source()
{
	@pool()
	{
		char[] compressed = deflate::compress(tmem, (char[])SAMPLE)!!;
		ChunkedReader src;
		src.init(compressed, 3);
		Inflater* inf = mem::tnew(Inflater);
		inf.init(&src, mem::temp_array(char, 8192));
		defer (void)inf.close();
		@assert_drains(inf, SAMPLE, "Inflater");
	};
}

fn void gzipreader_over_chunked_source()
{
	@pool()
	{
		char[] compressed = gzip::compress(tmem, (char[])SAMPLE)!!;
		for (sz chunk = 1; chunk <= 5; chunk++)
		{
			ChunkedReader src;
			src.init(compressed, chunk);
			GzipReader* gr = mem::tnew(GzipReader);
			gr.init(&src)!!;
			defer (void)gr.close();
			ByteWriter w;
			io::copy_to(gr, w.tinit())!!;
			assert(w.str_view() == SAMPLE, "gzip chunk %d: got '%s'", chunk, w.str_view());
		}
	};
}

fn void file_conformance()
{
	@pool()
	{
		String path = "partial_read_test.tmp";
		File f = file::open(path, "w")!!;
		f.write((char[])SAMPLE)!!;
		f.close()!!;
		defer (void)file::delete(path);

		File r = file::open(path, "r")!!;
		defer (void)r.close();
		@assert_drains(&r, SAMPLE, "File");
	};
}
