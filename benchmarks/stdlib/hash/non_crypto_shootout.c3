// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module non_crypto_benchmarks;


const usz COMMON_ITERATIONS = 1 << 18;

const char[] COMMON_1 = { 0xA5 };
const char[] COMMON_4 = { 0xA5, 0xA5, 0xA5, 0xA5, };
const char[] COMMON_8 = { [0..7] = 0xA5 };
const char[] COMMON_16 = { [0..15] = 0xA5 };
const char[] COMMON_32 = { [0..31] = 0xA5 };
const char[] COMMON_64 = { [0..63] = 0xA5 };
const char[] COMMON_128 = { [0..127] = 0xA5 };
const char[] COMMON_1024 = { [0..1023] = 0xA5 };


fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(COMMON_ITERATIONS + 3);
}


// =======================================================================================
module non_crypto_benchmarks @benchmark;

import std::hash;


fn void fnv64a_1() => fnv64a::hash(COMMON_1);
fn void fnv32a_1() => fnv32a::hash(COMMON_1);
fn void wyhash2_1() => wyhash2::hash(COMMON_1);
fn void metro64_1() => metro64::hash(COMMON_1);
fn void metro128_1() => metro128::hash(COMMON_1);
fn void a5hash_1() => a5hash::hash(COMMON_1);
fn void komi_1() => komi::hash(COMMON_1);

fn void fnv64a_4() => fnv64a::hash(COMMON_4);
fn void fnv32a_4() => fnv32a::hash(COMMON_4);
fn void wyhash2_4() => wyhash2::hash(COMMON_4);
fn void metro64_4() => metro64::hash(COMMON_4);
fn void metro128_4() => metro128::hash(COMMON_4);
fn void a5hash_4() => a5hash::hash(COMMON_4);
fn void komi_4() => komi::hash(COMMON_4);

fn void fnv64a_8() => fnv64a::hash(COMMON_8);
fn void fnv32a_8() => fnv32a::hash(COMMON_8);
fn void wyhash2_8() => wyhash2::hash(COMMON_8);
fn void metro64_8() => metro64::hash(COMMON_8);
fn void metro128_8() => metro128::hash(COMMON_8);
fn void a5hash_8() => a5hash::hash(COMMON_8);
fn void komi_8() => komi::hash(COMMON_8);

fn void fnv64a_16() => fnv64a::hash(COMMON_16);
fn void fnv32a_16() => fnv32a::hash(COMMON_16);
fn void wyhash2_16() => wyhash2::hash(COMMON_16);
fn void metro64_16() => metro64::hash(COMMON_16);
fn void metro128_16() => metro128::hash(COMMON_16);
fn void a5hash_16() => a5hash::hash(COMMON_16);
fn void komi_16() => komi::hash(COMMON_16);

fn void fnv64a_32() => fnv64a::hash(COMMON_32);
fn void fnv32a_32() => fnv32a::hash(COMMON_32);
// NOTE: wyhash2 cannot be used on inputs > 16 bytes.
fn void metro64_32() => metro64::hash(COMMON_32);
fn void metro128_32() => metro128::hash(COMMON_32);
fn void a5hash_32() => a5hash::hash(COMMON_32);
fn void komi_32() => komi::hash(COMMON_32);

fn void fnv64a_64() => fnv64a::hash(COMMON_64);
fn void fnv32a_64() => fnv32a::hash(COMMON_64);
// NOTE: wyhash2 cannot be used on inputs > 16 bytes.
fn void metro64_64() => metro64::hash(COMMON_64);
fn void metro128_64() => metro128::hash(COMMON_64);
fn void a5hash_64() => a5hash::hash(COMMON_64);
fn void komi_64() => komi::hash(COMMON_64);

fn void fnv64a_128() => fnv64a::hash(COMMON_128);
fn void fnv32a_128() => fnv32a::hash(COMMON_128);
// NOTE: wyhash2 cannot be used on inputs > 16 bytes.
fn void metro64_128() => metro64::hash(COMMON_128);
fn void metro128_128() => metro128::hash(COMMON_128);
fn void a5hash_128() => a5hash::hash(COMMON_128);
fn void komi_128() => komi::hash(COMMON_128);

fn void fnv64a_1024() => fnv64a::hash(COMMON_1024);
fn void fnv32a_1024() => fnv32a::hash(COMMON_1024);
// NOTE: wyhash2 cannot be used on inputs > 16 bytes.
fn void metro64_1024() => metro64::hash(COMMON_1024);
fn void metro128_1024() => metro128::hash(COMMON_1024);
fn void a5hash_1024() => a5hash::hash(COMMON_1024);
fn void komi_1024() => komi::hash(COMMON_1024);
