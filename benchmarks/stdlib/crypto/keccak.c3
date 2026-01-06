// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module crypto_hash_benchmarks;

char[] common_1mib;

fn void initialize_bench() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(256 + 3);

	common_1mib = mem::alloc_array(char, 1024*1024)[:1024*1024];
	common_1mib[..] = 0xA5;
}

fn void teardown_bench() @finalizer
{
	mem::free(common_1mib.ptr);
}


module crypto_hash_benchmarks @benchmark;

import std::hash, std::crypto;

fn void keccak_256() => keccak::xof(256, 32, common_1mib);
fn void keccak_512() => keccak::xof(512, 64, common_1mib);
fn void sha3_256() => sha3::hash(256, common_1mib);
fn void sha3_512() => sha3::hash(512, common_1mib);
fn void sha256_256() => sha256::hash(common_1mib);
fn void sha512_512() => sha512::hash(common_1mib);
