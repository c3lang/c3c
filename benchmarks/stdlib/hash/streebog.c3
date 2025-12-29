// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module streebog_bench;

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(256);
}

const char[] INPUT = { [0..1024*1024] = 0xA5 };
const char[*] EXPECTED_256 = x'694676905b7cf099755db1cc186f741f0fd1877aaaa4badcbfb305537f986971';
const char[*] EXPECTED_512 = x'fe21d08857ea97e79035d1e5c9ba5130786e8d1875bc74d628349560d94d6bdff0b0dcd2f6347eb8b3f0239b6cca76b5028c0ff45f631fcdf77b1d551dd079f3';


module streebog_bench @benchmark;

import std::hash;

fn void get_in_the_bog_256()
{
    runtime::@start_benchmark();
    char[*] myset = streebog::hash_256(INPUT);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void get_in_the_bog_512()
{
    runtime::@start_benchmark();
    char[*] myset = streebog::hash_512(INPUT);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void compared_with_sha256()
{
    runtime::@start_benchmark();
    char[*] myset = sha256::hash(INPUT);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}

fn void compared_with_whirlpool()
{
    runtime::@start_benchmark();
    char[*] myset = whirlpool::hash(INPUT);
    runtime::@end_benchmark();
    mem::zero_volatile(myset[..]);
}
