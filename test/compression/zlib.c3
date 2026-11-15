module std::compression::zlib_test;
import std::compression::zlib @public;
import std::io, std::math, std::compression::deflate, std::hash::adler32;

// Deterministic pseudo random bytes.
macro char[] zlib_test_random(Allocator allocator, sz len)
{
	char[] data = alloc::alloc_array(allocator, char, len);
	uint state = 12345;
	for (sz i = 0; i < len; i++)
	{
		state = state * 1103515245 + 12345;
		data[i] = (char)(state >> 16);
	}
	return data;
}

// Compressible text made of a few words.
macro char[] zlib_test_words(Allocator allocator, sz len)
{
	String[8] words = { "alpha ", "beta ", "gamma ", "delta ", "epsilon ", "zeta ", "eta ", "theta " };
	char[] data = alloc::alloc_array(allocator, char, len);
	uint state = 12345;
	sz pos = 0;
	while (pos < len)
	{
		state = state * 1103515245 + 12345;
		String w = words[(state >> 16) & 7];
		sz n = math::min((sz)w.len, len - pos);
		data[pos:n] = w[:n];
		pos += n;
	}
	return data;
}

macro void @expect_fault(#expr, expected)
{
	if (catch err = #expr)
	{
		test::eq(err, expected);
		return;
	}
	assert(false, "expected a fault");
}

// The streams below were produced by Python's zlib.
fn void zlib_python_vectors() @test
{
	char[*] empty = x"789C030000000001";
	char[*] hello = x"789CCB48CDC9C95728CF2FCA4901001A0B045D";
	char[*] fast = x"78014B4C4A4E444500417C06E5";
	char[*] a1000 = x"78DA4B4C1C05A360140C770000F9D87AF8";
	char[*] win9 = x"1895CB48CDC9C957C8402701680308B1";
	char[*] stored = x"7801012C01D3FEDC0465AA1FAD1D5ADAE5AC1B1E5F1370796CFD10FF19AF601D04ACB41D022B4678733AF2DF5FAEB70859D1EE3910CB4895B5CC892911FF06B6622EDF3CF935FD4B9428CA097C44B3025E965FB3EA6DACD42D816E69AFE0E6874C9C04E7D2365D2C60C9EAF479F686A0EB9326E46212D50DCBB377156A6A3A68BA8EDB7408469EF3CEB30AF8D0DD68BBF85FFA24F2D2FC1887FB5C87BAB43832A59B1B3D107CF778D67FE26DF81191297E9395CB12C557CE5AF1D41618D719BC045B7E9965F1A29471C42AAC6AA938C475C7AD3238021F053B2C991AFCEB15DECF68BAE07CBCD61E971B9A0B9DBE9763D392FCAFDFA28C97234562EBDD076570FF58896ACFF7CAEE3F1CE9E40A68E5DE938D389C7DBDD75B09D4E7E233443F4A8CC4A190D6B8B8DC615FD18E28BE590EAA501B9AAD9AE4";
	char[*] words = x"789C8596DB6EC2400C447F855FCBAA51A9042D52F394AFAFC0497CC69ED007C2DE7C1B8FBD3B3F7EBF6E3FDF9779FB5FE765BA7C4EF7FB7459AECFF17A7C6235BEAF855DE663BE3D67CB2E12F3E9F6B84E97C1832FA9588F9323856259F4D67F789126432EE6F430B4C6B75A1987ED6340357045FC49B1A1BAD32FAC5328756D8E6F9B612944870B9D716924DCB1A8AD3D4862D03DADFFB19BB6C50A80DC4E30DEFD47543365FBD1462820145F509087E34BF0CEA0DD1D49E3C42DCE20DD9D050282F15BCD3135164693F716343C511D357190AFBC1E1AF85A235F95C08E81AE543D3D8FEDA49CE384A0A7B3D1438687C83F81C3A6226E3C000CB510A8B895C269C95985CCA2DA6737723B50D7F4234E26A455A0B1E1BB9162CFC6FB8E8359A1745914D2F6FF45203E25B7599472843C1489638901B188A560BB0E00FCCECF56A8A3AA50B7DA358861AD148799EF7A86DC67A83118E9200C48B14BBF6A426A0A4CC95922BF6B9AAE3A6C0644B91B313C65BC6B51D4BB6AB0A450AB2D6040B3E67DA2B74F4F01F33D6AD804024E2F66072E926EED5DD1EEB83ACC29DB9747A013D43CF0DA83A37107F5945162EBE4ED55C9E33B3772D19E0FD59AE4F2F42CC072C96F573A3341817A97F7A605CBF5697D84F907D22D4162";

	@pool()
	{
		assert(zlib::uncompress(tmem, &empty)!!.len == 0);
		assert((String)zlib::uncompress(tmem, &hello)!! == "hello world");
		assert((String)zlib::uncompress(tmem, &fast)!! == "abcabcabcabcabcabc");
		assert((String)zlib::uncompress(tmem, &win9)!! == "hello hello hello hello");

		char[] ones = zlib::uncompress(tmem, &a1000)!!;
		assert(ones.len == 1000);
		foreach (c : ones) assert(c == 'a');

		assert(zlib::uncompress(tmem, &stored)!! == zlib_test_random(tmem, 300));
		assert(zlib::uncompress(tmem, &words)!! == zlib_test_words(tmem, 3000));
	};
}

fn void zlib_roundtrip_levels() @test
{
	sz[5] lengths = { 0, 1, 100, 5553, 70000 };
	@pool()
	{
		foreach (len : lengths)
		{
			char[][2] inputs = { zlib_test_words(tmem, len), zlib_test_random(tmem, len) };
			foreach (data : inputs)
			{
				for (int level = deflate::MIN_LEVEL; level <= deflate::MAX_LEVEL; level++)
				{
					char[] z = zlib::compress(tmem, data, level)!!;
					assert(z[0] == 0x78, "CMF");
					assert(((uint)z[0] * 256 + z[1]) % 31 == 0, "FCHECK");
					assert((z[1] & 0x20) == 0, "FDICT must not be set");
					uint adler = (uint)z[^4] << 24 | (uint)z[^3] << 16 | (uint)z[^2] << 8 | z[^1];
					assert(adler == adler32::hash(data), "trailer");
					assert(zlib::uncompress(tmem, z)!! == data, "round trip");
				}
				assert(zlib::uncompress(tmem, zlib::compress(tmem, data)!!)!! == data, "default level");
			}
		}
	};
}

fn void zlib_header_level_hint() @test
{
	@pool()
	{
		char[] data = zlib_test_words(tmem, 1000);
		// Default level has the same header as zlib's default.
		char[] z = zlib::compress(tmem, data)!!;
		assert(z[0] == 0x78 && z[1] == 0x9C);
		assert(zlib::compress(tmem, data, deflate::MIN_LEVEL)!![1] >> 6 == 0);
		assert(zlib::compress(tmem, data, deflate::MAX_LEVEL)!![1] >> 6 == 3);
	};
}

fn void zlib_writer_chunks() @test
{
	sz[4] steps = { 1, 7, 4093, 100000 };
	@pool()
	{
		char[] data = zlib_test_words(tmem, 50000);
		foreach (step : steps)
		{
			ByteWriter bw;
			bw.tinit();
			ZlibWriter w;
			w.init(tmem, &bw, 6)!!;
			sz pos = 0;
			while (pos < (sz)data.len)
			{
				sz n = math::min(step, (sz)data.len - pos);
				if (n == 1)
				{
					w.write_byte(data[pos])!!;
				}
				else
				{
					w.write(data[pos:n])!!;
				}
				pos += n;
			}
			w.close()!!;
			assert(zlib::uncompress(tmem, bw.array_view())!! == data, "chunked write");
		}
	};
}

fn void zlib_reader_small_reads() @test
{
	@pool()
	{
		char[] data = zlib_test_words(tmem, 40000);
		char[] z = zlib::compress(tmem, data)!!;
		ByteReader br = io::wrap_bytes(z);
		ZlibReader r;
		r.init(&br, tmem)!!;
		defer (void)r.close();

		// An empty read does not finish the stream.
		char[] none = {};
		assert(r.read(none)!! == 0);

		ByteWriter bw;
		bw.tinit();
		sz[3] sizes = { 1, 3, 1000 };
		sz i = 0;
		char[1000] buf;
		while (sz n = r.read(buf[:sizes[i++ % 3]])!!)
		{
			bw.write(buf[:n])!!;
		}
		assert(bw.array_view() == data, "small reads");
		assert(r.read(buf[:10])!! == 0, "reads after the end return 0");
	};
}

fn void zlib_trailing_data_is_left_in_source() @test
{
	@pool()
	{
		char[] data = zlib_test_words(tmem, 30000);
		char[] z = zlib::compress(tmem, data)!!;
		char[] all = alloc::alloc_array(tmem, char, z.len + 5);
		all[:z.len] = z[..];
		all[z.len:5] = "tail!"[..];

		ByteReader br = io::wrap_bytes(all);
		ByteWriter bw;
		bw.tinit();
		zlib::uncompress_stream(&bw, &br)!!;
		assert(bw.array_view() == data);
		char[5] rest;
		assert(io::read_buffer(&br, &rest)!! == 5);
		assert((String)rest[..] == "tail!", "unused input must be handed back");
	};
}

fn void zlib_header_errors() @test
{
	@pool()
	{
		char[] good = zlib::compress(tmem, "some data to compress some data")!!;

		// Too short.
		char[1] one = { 0x78 };
		@expect_fault(zlib::uncompress(tmem, &one), zlib::CORRUPT_HEADER);
		char[] nothing = {};
		@expect_fault(zlib::uncompress(tmem, nothing), zlib::CORRUPT_HEADER);

		// FCHECK wrong.
		char[] bad = alloc::clone_slice(tmem, good);
		bad[1] ^= 1;
		@expect_fault(zlib::uncompress(tmem, bad), zlib::CORRUPT_HEADER);

		// Method 15 with a valid FCHECK.
		bad = alloc::clone_slice(tmem, good);
		bad[0] = 0x7F;
		bad[1] = 0;
		bad[1] = (char)(31 - ((uint)bad[0] * 256) % 31) % 31;
		@expect_fault(zlib::uncompress(tmem, bad), zlib::UNSUPPORTED_METHOD);

		// Window larger than 32 KiB (CINFO 8) with a valid FCHECK.
		bad = alloc::clone_slice(tmem, good);
		bad[0] = 0x88;
		bad[1] = 0;
		bad[1] = (char)(31 - ((uint)bad[0] * 256) % 31) % 31;
		@expect_fault(zlib::uncompress(tmem, bad), zlib::CORRUPT_HEADER);

		// Preset dictionary (FDICT) with a valid FCHECK.
		bad = alloc::clone_slice(tmem, good);
		bad[0] = 0x78;
		bad[1] = 0x20;
		bad[1] += (char)(31 - ((uint)bad[0] * 256 + bad[1]) % 31) % 31;
		@expect_fault(zlib::uncompress(tmem, bad), zlib::DICTIONARY_REQUIRED);
	};
}

fn void zlib_data_errors() @test
{
	@pool()
	{
		char[] data = zlib_test_words(tmem, 20000);
		char[] good = zlib::compress(tmem, data)!!;

		// Corrupt checksum.
		char[] bad = alloc::clone_slice(tmem, good);
		bad[^1] ^= 0x55;
		@expect_fault(zlib::uncompress(tmem, bad), zlib::CORRUPT_DATA);

		// Missing and truncated checksum.
		for (sz cut = 1; cut <= 4; cut++)
		{
			@expect_fault(zlib::uncompress(tmem, good[:good.len - cut]), zlib::CORRUPT_DATA);
		}

		// Truncated DEFLATE data and a flipped byte in the middle: an error, never a wrong result.
		sz[3] cuts = { (sz)good.len / 2, (sz)good.len / 4, 10 };
		foreach (cut : cuts)
		{
			assert(@catch(zlib::uncompress(tmem, good[:cut])), "truncated input must fail");
		}
		bad = alloc::clone_slice(tmem, good);
		bad[bad.len / 2] ^= 0xFF;
		char[]? res = zlib::uncompress(tmem, bad);
		if (try res) assert(res != data, "corruption must not go unnoticed");
	};
}

// An input stream without seek that hands out at most `chunk` bytes per read call.
struct NoSeekStream (InStream)
{
	char[] data;
	sz pos;
	sz chunk;
}

fn sz? NoSeekStream.read(&self, char[] buf) @dynamic
{
	sz data_len = (sz)self.data.len;
	if (self.pos >= data_len) return io::EOF~;
	sz n = math::min(math::min((sz)buf.len, self.chunk), data_len - self.pos);
	buf[:n] = self.data[self.pos:n];
	self.pos += n;
	return n;
}

fn char? NoSeekStream.read_byte(&self) @dynamic
{
	if (self.pos >= self.data.len) return io::EOF~;
	return self.data[self.pos++];
}

fn void zlib_stream_without_seek() @test
{
	sz[4] chunks = { 1, 5, 4096, 1000000 };
	@pool()
	{
		char[] data = zlib_test_words(tmem, 60000);
		char[] z = zlib::compress(tmem, data)!!;
		foreach (chunk : chunks)
		{
			NoSeekStream src = { .data = z, .chunk = chunk };
			ByteWriter bw;
			bw.tinit();
			zlib::uncompress_stream(&bw, &src)!!;
			assert(bw.array_view() == data, "source without seek");

			// The checksum is still verified.
			char[] bad = alloc::clone_slice(tmem, z);
			bad[^2] ^= 0x01;
			NoSeekStream bad_src = { .data = bad, .chunk = chunk };
			ByteWriter bad_out;
			bad_out.tinit();
			@expect_fault(zlib::uncompress_stream(&bad_out, &bad_src), zlib::CORRUPT_DATA);
		}
	};
}
