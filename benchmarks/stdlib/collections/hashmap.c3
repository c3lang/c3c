// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Some benchmark test ideas are sourced from this article on C++ hashmap benchmarking:
//   https://martin.ankerl.com/2022/08/27/hashmap-bench-01/
//
module hashmap_benchmarks;

import std::collections::map;
import std::math::random;


const DEFAULT_ITERATIONS = 16384;

Lcg64Random rand;

HashMap { int, int } modifying_numbers_random;

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(DEFAULT_ITERATIONS);

	// TODO: Cannot take the address of a @benchmark function. If we could, we could pass &insert_erase as a fn ptr and use the $qnameof CT eval internally.
	set_benchmark_func_iterations($qnameof(insert_erase), 32);
	set_benchmark_func_iterations($qnameof(random_access), 1024);

    random::seed(&rand, 0x4528_21e6_38d0_1377);

    for (usz i = 0; i < 1_000; ++i) modifying_numbers_random.set(rand.next_int(), rand.next_int());
}


// ==============================================================================================
module hashmap_benchmarks @benchmark;

import std::collections::map;

import std::math::random;
import std::encoding::base64;


fn void generic_hash_speeds()
{
	(char){}.hash();
	(char[<100>]){}.hash();
	(char[100]){}.hash();
	(ichar){}.hash();
	(ichar[<100>]){}.hash();
	(ichar[100]){}.hash();
	(short){}.hash();
	(short[<100>]){}.hash();
	(short[100]){}.hash();
	(ushort){}.hash();
	(ushort[<100>]){}.hash();
	(ushort[100]){}.hash();
	(int){}.hash();
	(int[<100>]){}.hash();
	(int[100]){}.hash();
	(uint){}.hash();
	(uint[<100>]){}.hash();
	(uint[100]){}.hash();
	(long){}.hash();
	(long[<20>]){}.hash();
	(long[100]){}.hash();
	(ulong){}.hash();
	(ulong[<20>]){}.hash();
	(ulong[100]){}.hash();
	(int128){}.hash();
	(int128[<20>]){}.hash();
	(int128[100]){}.hash();
	(uint128){}.hash();
	(uint128[<20>]){}.hash();
	(uint128[100]){}.hash();
	(bool){}.hash();
	(bool[<100>]){}.hash();
	(bool[100]){}.hash();
	String x = "abc";
	char[] y = "abc";
	assert(x.hash() == y.hash());
	String z1 = "This is a much longer string than the above value because longer values lead to longer hashing times.";
	char[] z2 = "This is a much longer string than the above value because longer values lead to longer hashing times.";
	assert(z1.hash() == z2.hash());
	assert(int.typeid.hash());
}


fn void hash_speeds_of_many_random_values() => @pool()
{
	var $arrsz = 10_000;
	uint fake_checksum;

	char[] chars = allocator::new_array(tmem, char, $arrsz)[:$arrsz];
	foreach (&v : chars) *v = (char)random::next(&rand, uint.max);

	ushort[] shorts = allocator::new_array(tmem, ushort, $arrsz)[:$arrsz];
	foreach (&v : shorts) *v = (ushort)random::next(&rand, uint.max);

	uint[] ints = allocator::new_array(tmem, uint, $arrsz)[:$arrsz];
	foreach (&v : ints) *v = random::next(&rand, uint.max);

	ulong[] longs = allocator::new_array(tmem, ulong, $arrsz)[:$arrsz];
	foreach (&v : longs) *v = (ulong)random::next(&rand, uint.max);

	uint128[] vwideints = allocator::new_array(tmem, uint128, $arrsz)[:$arrsz];
	foreach (&v : vwideints) *v = (uint128)random::next(&rand, uint.max);

	char[48][] zstrs = allocator::new_array(tmem, char[48], $arrsz)[:$arrsz];
	String[$arrsz] strs;
	foreach (x, &v : zstrs)
	{
		foreach (&c : (*v)[:random::next(&rand, 48)]) *c = (char)random::next(&rand, char.max);
		strs[x] = ((ZString)&v[0]).str_view();
	}

	runtime::@start_benchmark();
	foreach (v : chars) fake_checksum += v.hash();
	foreach (v : shorts) fake_checksum += v.hash();
	foreach (v : ints) fake_checksum += v.hash();
	foreach (v : longs) fake_checksum += v.hash();
	foreach (v : vwideints) fake_checksum += v.hash();
	foreach (v : strs) fake_checksum += v.hash();
	runtime::@end_benchmark();
}


fn void modifying_numbers_init_from_map() => @pool()
{
	HashMap { int, int } v;
	v.tinit_from_map(&modifying_numbers_random);
	v.free();
}


fn void insert_erase() => @pool()
{
	uint iters = 1_000_000;
	HashMap { int, int } v;
	v.tinit();

	runtime::@start_benchmark();
	for (int i = 0; i < iters; ++i) v[i] = i;
	for (int i = 0; i < iters; ++i) v.remove(i);

	runtime::@end_benchmark();

	v.free();
}


fn void random_access() => @pool()
{
	HashMap { int, int } v;
	v.tinit();

	uint bound = 10_000;
	usz pseudo_checksum = 0;

	for (uint i = 0; i < bound; ++i) v[i] = i;

	runtime::@start_benchmark();
	for (uint i = 0; i < 1_000_000; ++i) pseudo_checksum += (v[i.hash() % bound] ?? 0);
	runtime::@end_benchmark();

	v.free();
}


fn void random_access_erase() => @pool()
{
	HashMap { int, int } v;
	v.tinit();

	uint bound = 10_000;

	for (uint i = 0; i < bound; ++i) v[i] = i;

	runtime::@start_benchmark();
	for (uint i = 0; i < bound; ++i)
	{
		v[i.hash() % bound] = i;   // supplant an entry

		v.remove(random::next(&rand, bound));   // remove a random entry
	}
	runtime::@end_benchmark();

	v.free();
}


fn void random_access_string_keys() => @pool()
{
	HashMap { String, ulong } v;
	v.tinit();

	usz pseudo_checksum = 0;
	String[5_000] saved;

	for (usz i = 0; i < saved.len; ++i)
	{
		ulong hash = i.hash();
		String b64key = base64::tencode(@as_char_view(hash));

		v[b64key] = hash;

		if (i < saved.len) saved[i] = b64key;
	}

	runtime::@start_benchmark();
	for (usz i = 0; i < saved.len; ++i)
	{
		pseudo_checksum += v[ saved[random::next(&rand, saved.len)] ]!! % 512;
	}
	runtime::@end_benchmark();

	v.free();
}
