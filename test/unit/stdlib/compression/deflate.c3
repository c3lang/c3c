module deflate_test @test;

import std::compression::deflate;
import std::core::mem;
import std::io, std::io::file;
import std::math;

fn void test_deflate_basic()
{
	String original = "Hello, world! This is a test of the DEFLATE compression algorithm.";
	char[] compressed = deflate::compress((char[])original)!!;
	defer free(compressed.ptr);

	char[] decompressed = deflate::decompress(compressed)!!;
	defer free(decompressed.ptr);

	assert((String)decompressed == original, "Decompressed data does not match original");
}

fn void test_deflate_repetitive()
{
	// 5000 bytes of repetitive data should compress very well
	usz len = 5000;
	char[] original = mem::malloc(len)[:len];
	defer free(original.ptr);

	for (usz i = 0; i < len; i++)
	{
		original[i] = (char)((i % 10) + '0');
	}

	char[] compressed = deflate::compress(original)!!;
	defer free(compressed.ptr);

	// Check that we actually achieved some compression
	assert(compressed.len < len / 10, "Repetitive data should compress well");

	char[] decompressed = deflate::decompress(compressed)!!;
	defer free(decompressed.ptr);

	assert(decompressed.len == original.len, "Length mismatch");
	assert((String)decompressed == (String)original, "Data mismatch");
}

fn void test_deflate_empty()
{
	char[] original = {};
	char[] compressed = deflate::compress(original)!!;
	defer free(compressed.ptr);

	char[] decompressed = deflate::decompress(compressed)!!;
	defer free(decompressed.ptr);

	assert(decompressed.len == 0, "Expected empty decompression");
}

fn void test_deflate_large_repetitive() @if($feature(SLOW_TESTS))
{
	// Test larger buffer to trigger reallocs in inflater
	usz len = 100000;
	char[] original = mem::malloc(len)[:len];
	defer free(original.ptr);

	mem::set(original.ptr, (char)'A', len);

	char[] compressed = deflate::compress(original)!!;
	defer free(compressed.ptr);

	char[] decompressed = deflate::decompress(compressed)!!;
	defer free(decompressed.ptr);

	assert(decompressed.len == len, "Length mismatch");
	assert(decompressed[0] == 'A' && decompressed[len-1] == 'A', "Data mismatch");
}

fn void test_deflate_random_ish()
{
	// Data that doesn't compress well
	usz len = 1024;
	char[] original = mem::malloc(len)[:len];
	defer free(original.ptr);

	for (usz i = 0; i < len; i++)
	{
		original[i] = (char)(i & 0xFF);
	}

	char[] compressed = deflate::compress(original)!!;
	defer free(compressed.ptr);

	char[] decompressed = deflate::decompress(compressed)!!;
	defer free(decompressed.ptr);

	assert((String)decompressed == (String)original, "Data mismatch");
}

fn void test_deflate_corrupted()
{
	char[] compressed = deflate::compress((char[])"Some data")!!;
	defer free(compressed.ptr);

	// Corrupt the block type (bits 1-2 of first byte) to 3 (reserved/invalid)
	compressed[0] |= 0x06;

	char[]? decompressed = deflate::decompress(compressed);
	assert(!@ok(decompressed), "Expected decompression to fail for corrupted data");
}

fn void test_deflate_stream()
{
	@pool()
	{
		String base = "This is a streaming test for DEFLATE. ";
		usz base_len = base.len;
		usz count = 50;
		char[] original_arr = mem::malloc(base_len * count)[:base_len * count];
		defer free(original_arr.ptr);
		for (usz i = 0; i < count; i++) {
			mem::copy(original_arr.ptr + i * base_len, base.ptr, base_len);
		}
		String original = (String)original_arr;

		char[] compressed = deflate::compress(original_arr)!!;
		defer free(compressed.ptr);

		// Use a temporary file on disk to test the streaming interface
		File f = file::open("unittest_stream_deflate.bin", "wb+")!!;
		defer { (void)f.close(); (void)file::delete("unittest_stream_deflate.bin"); }
		
		f.write(compressed)!!;
		f.seek(0, Seek.SET)!!;

		// Decompress using stream
		File out_f = file::open("unittest_stream_out.bin", "wb+")!!;
		defer { (void)out_f.close(); (void)file::delete("unittest_stream_out.bin"); }

		deflate::decompress_stream(&f, &out_f)!!;
		
		usz out_size = out_f.seek(0, Seek.CURSOR)!!;
		assert(out_size == original.len, "Length mismatch in streaming decompression");

		out_f.seek(0, Seek.SET)!!;
		char[] result = mem::malloc(out_size)[:out_size];
		defer free(result.ptr);
		out_f.read(result)!!;

		assert((String)result == original, "Data mismatch in streaming decompression");
	};
}
