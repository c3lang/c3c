module md5_bench;

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(128);

    input = mem::alloc_array(char, BUFSZ);
    input[:BUFSZ] = (char[]){ [0..BUFSZ-1] = 0xA5 }[..];
    input_slice = input[:BUFSZ];
}

fn void teardown_bench() @finalizer
{
    mem::free(input);
    input = null;
}

char* input;
char[] input_slice;
const usz BUFSZ = 1024 * 1024;

module md5_bench @benchmark;

import std::hash;

fn void md5_hash()
{
    runtime::@start_benchmark();
    char[*] myset = md5::hash(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void compared_with_sha256()
{
    runtime::@start_benchmark();
    char[*] myset = sha256::hash(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}
