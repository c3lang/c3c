// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module ripemd_bench;

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(256);

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

module ripemd_bench @benchmark;

import std::hash;

fn void ripemd_128()
{
    runtime::@start_benchmark();
    char[*] myset = ripemd::hash{128}(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void ripemd_160()
{
    runtime::@start_benchmark();
    char[*] myset = ripemd::hash{160}(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void ripemd_256()
{
    runtime::@start_benchmark();
    char[*] myset = ripemd::hash{256}(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void ripemd_320()
{
    runtime::@start_benchmark();
    char[*] myset = ripemd::hash{320}(input_slice);
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

fn void compared_with_whirlpool()
{
    runtime::@start_benchmark();
    char[*] myset = whirlpool::hash(input_slice);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}