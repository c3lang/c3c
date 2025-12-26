// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module crypto_hash_benchmarks;

import std::collections::pair;


const usz COMMON_ITERATIONS = 1 << 17;

char* common_1mib_ptr;
char[] common_16;
char[] common_256;
char[] common_4kib;
char[] common_1mib;

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(COMMON_ITERATIONS + 3);

	common_1mib_ptr = mem::alloc_array(char, 1024*1024);
	common_1mib = common_1mib_ptr[:1024*1024];
	common_1mib[..] = 0xA5;

	common_16 = common_1mib[:16];
	common_256 = common_1mib[:256];
	common_4kib = common_1mib[:4096];

	static String[] function_prefixes = {
		$qnameof(md5_16)[..^4],
		$qnameof(sha1_16)[..^4],
		$qnameof(sha2_256_16)[..^4],
		$qnameof(sha2_512_16)[..^4],
		$qnameof(blake2s_256_16)[..^4],
		$qnameof(blake2b_256_16)[..^4],
		$qnameof(blake3_16)[..^4],
		$qnameof(ripemd_160_16)[..^4],
		$qnameof(whirlpool_16)[..^4],
		$qnameof(streebog_256_16)[..^4],
		$qnameof(streebog_512_16)[..^4],
	};

	static Pair{ String, uint }[] to_iters = {
		{ "_4kib", 1 << 15 },
		{ "_1mib", 1024 },
	};

	foreach (p : to_iters)
	{
		foreach (name : function_prefixes) set_benchmark_func_iterations(name.tconcat(p.first), p.second);
	}
}

fn void teardown_bench() @finalizer
{
	mem::free(common_1mib_ptr);
}


// =======================================================================================
module crypto_hash_benchmarks @benchmark;

import std::hash;


fn void md5_16() => md5::hash(common_16);
fn void sha1_16() => sha1::hash(common_16);
fn void sha2_256_16() => sha256::hash(common_16);
fn void sha2_512_16() => sha512::hash(common_16);
fn void blake2s_256_16() => blake2::s(256, common_16);
fn void blake2b_256_16() => blake2::b(256, common_16);
fn void blake3_16() => blake3::hash(common_16);
fn void ripemd_160_16() => ripemd::hash{160}(common_16);
fn void whirlpool_16() => whirlpool::hash(common_16);
fn void streebog_256_16() => streebog::hash_256(common_16);
fn void streebog_512_16() => streebog::hash_512(common_16);

fn void md5_256() => md5::hash(common_256);
fn void sha1_256() => sha1::hash(common_256);
fn void sha2_256_256() => sha256::hash(common_256);
fn void sha2_512_256() => sha512::hash(common_256);
fn void blake2s_256_256() => blake2::s(256, common_256);
fn void blake2b_256_256() => blake2::b(256, common_256);
fn void blake3_256() => blake3::hash(common_256);
fn void ripemd_160_256() => ripemd::hash{160}(common_256);
fn void whirlpool_256() => whirlpool::hash(common_256);
fn void streebog_256_256() => streebog::hash_256(common_256);
fn void streebog_512_256() => streebog::hash_512(common_256);

fn void md5_4kib() => md5::hash(common_4kib);
fn void sha1_4kib() => sha1::hash(common_4kib);
fn void sha2_256_4kib() => sha256::hash(common_4kib);
fn void sha2_512_4kib() => sha512::hash(common_4kib);
fn void blake2s_256_4kib() => blake2::s(256, common_4kib);
fn void blake2b_256_4kib() => blake2::b(256, common_4kib);
fn void blake3_4kib() => blake3::hash(common_4kib);
fn void ripemd_160_4kib() => ripemd::hash{160}(common_4kib);
fn void whirlpool_4kib() => whirlpool::hash(common_4kib);
fn void streebog_256_4kib() => streebog::hash_256(common_4kib);
fn void streebog_512_4kib() => streebog::hash_512(common_4kib);

fn void md5_1mib() => md5::hash(common_1mib);
fn void sha1_1mib() => sha1::hash(common_1mib);
fn void sha2_256_1mib() => sha256::hash(common_1mib);
fn void sha2_512_1mib() => sha512::hash(common_1mib);
fn void blake2s_256_1mib() => blake2::s(256, common_1mib);
fn void blake2b_256_1mib() => blake2::b(256, common_1mib);
fn void blake3_1mib() => blake3::hash(common_1mib);
fn void ripemd_160_1mib() => ripemd::hash{160}(common_1mib);
fn void whirlpool_1mib() => whirlpool::hash(common_1mib);
fn void streebog_256_1mib() => streebog::hash_256(common_1mib);
fn void streebog_512_1mib() => streebog::hash_512(common_1mib);
