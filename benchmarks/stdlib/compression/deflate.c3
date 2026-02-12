module deflate_benchmarks;
import std::compression::deflate;

const uint SMALL_ITERATIONS = 50000;
const uint LARGE_ITERATIONS = 100;

// Data to compress
const char[] SMALL_DATA = { [0..1023] = 'A' };
const char[] LARGE_DATA = { [0..1048575] = 'B' };

char[] small_compressed;
char[] large_compressed;

fn void initialize_bench() @init
{
	small_compressed = deflate::compress(SMALL_DATA, mem)!!;
	large_compressed = deflate::compress(LARGE_DATA, mem)!!;
	set_benchmark_warmup_iterations(2);
	set_benchmark_max_iterations(10);

	set_benchmark_func_iterations($qnameof(deflate_compress_small), SMALL_ITERATIONS);
	set_benchmark_func_iterations($qnameof(deflate_decompress_small), SMALL_ITERATIONS);
	set_benchmark_func_iterations($qnameof(deflate_compress_large), LARGE_ITERATIONS);
	set_benchmark_func_iterations($qnameof(deflate_decompress_large), LARGE_ITERATIONS);
}

// =======================================================================================
module compression_benchmarks @benchmark;

import std::compression::deflate;
import std::core::mem;

fn void deflate_compress_small() => @pool()
{
	char[]? compressed = deflate::compress(SMALL_DATA, tmem);
}

fn void deflate_decompress_small() => @pool()
{
	char[]? decompressed = deflate::decompress(small_compressed, tmem);
}

fn void deflate_compress_large() => @pool()
{
	char[]? compressed = deflate::compress(LARGE_DATA, tmem);
}

fn void deflate_decompress_large() => @pool()
{
	char[]? decompressed = deflate::decompress(large_compressed, tmem);
}
