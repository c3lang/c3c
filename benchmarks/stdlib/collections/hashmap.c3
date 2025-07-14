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


Lcg64Random rand;

HashMap { int, int } modifying_numbers_random;

fn void bench_setup() @init
{
	// set_benchmark_warmup_iterations(3);
	// set_benchmark_max_iterations(1 + 3);
	set_benchmark_max_iterations(1);
	// TODO ^ increase max iter to 2048 when finished

    random::seed(&rand, 0x4528_21E6_38D0_1377);

    for (usz i = 0; i < 1000; ++i) modifying_numbers_random.set(rand.next_int(), rand.next_int());
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
	assert(int.typeid.hash());
}


fn void modifying_numbers_init_from_map()
{
	$for var $x = 0; $x < 100; ++$x :
	{   // scope 'v'
		HashMap { int, int } v;
		v.init_from_map(mem, &modifying_numbers_random);
		v.free();
	}
	$endfor
}


fn void insert_erase_100k_int_100k()
{
	HashMap { int, int } v;
	v.init(mem);

	runtime::@start_benchmark();
	for (int i = 0; i < 100_000; ++i) v[i] = i;
	v.clear();

	for (int i = 0; i < 100_000; ++i) v[i] = i;
	for (int i = 0; i < 100_000; ++i) v.remove(i);

	runtime::@end_benchmark();
	v.free();
}


fn void random_access_1m_int_100k()
{
	HashMap { int, int } v;
	v.tinit();

	uint bound = 100_000;
	usz pseudo_checksum = 0;

	for (uint i = 0; i < bound; ++i) v[i] = i;

	runtime::@start_benchmark();
	for (uint i = 0; i < 1_000_000; ++i) pseudo_checksum += (v[i.hash() % bound] ?? 0);
	runtime::@end_benchmark();

	v.free();
}


fn void random_access_erase_1m_int_100k()
{
	HashMap { int, int } v;
	v.init(mem);

	uint bound = 100_000;

	for (uint i = 0; i < bound; ++i) v[i] = i;

	runtime::@start_benchmark();
	for (uint i = 0; i < 1_000_000; ++i)
	{
		v[i.hash() % bound] = i;   // supplant an entry

		v.remove(random::next(rand, bound));   // remove a random entry
	}
	runtime::@end_benchmark();

	v.free();
}


// TODO!
// fn void iterator()
// {
// 	HashMap { int, int } v;
// 	v.init(mem);

// 	uint bound = 100_000;

// 	for (uint i = 0; i < bound; ++i) v[i] = i;
// }


fn void random_access_string_keys_100k()
{
	HashMap { String, ulong } v;
	v.init(mem);

	usz pseudo_checksum = 0;
	String[5_000] saved;

	for (usz i = 0; i < 100_000; ++i)
	{
		ulong hash = i.hash();
		String b64key = base64::tencode(@as_char_view(hash));

		v[b64key] = hash;

		if (i < saved.len) saved[i] = b64key;
	}

	runtime::@start_benchmark();
	for (usz i = 0; i < 100_000; ++i)
	{
		pseudo_checksum += v[ saved[random::next(rand, saved.len)] ]!! % 512;
	}
	runtime::@end_benchmark();

	v.free();
}
