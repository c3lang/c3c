// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// An implementation of Wang Yi's wyhash(2) algorithm in C3:
//    https://github.com/wangyi-fudan/wyhash
//
module std::hash::wyhash2;


fn ulong wyr3(char* in, usz len) @inline
	=> ((ulong)in[0] << 16) | ((ulong)in[len >> 1] << 8) | (ulong)in[len - 1];


// See: https://docs.google.com/spreadsheets/d/1HmqDj-suH4wBFNg7etwE8WVBlfCufvD5-gAnIENs94k/edit?gid=1915335726#gid=1915335726
//   Credit to article:
//   https://medium.com/@tprodanov/benchmarking-non-cryptographic-hash-functions-in-rust-2e6091077d11
//
// wyhash2 has a >90% chance of collisions when its input data is above 16 bytes in length.
//   However, it is the fastest performing and most evenly randomized hash for very low-length inputs,
//   making it an ideal candidate for hashing primitive data types quickly and making things like hash
//   tables even faster. Therefore, a 16-byte input limit is imposed on all calls to the hash function.
//
<*
@require input.len <= 16 : `wyhash2 is not useable for inputs over 16 bytes in length.`
*>
fn ulong hash(char[] input, ulong seed = 0)
{
	seed ^= 0xa076_1d64_78bd_642f;

	ulong a, b;

	if (@likely(input.len <= 8))   // more likely to encounter 8-byte or lower type here
	{
		if (@likely(input.len >= 4))
		{
			a = (ulong)mem::load((uint*)input.ptr, 1);   // first 4 bytes widened to a u64
			b = (ulong)mem::load((uint*)&input[^4], 1);   // a walking 4-byte window based on input.len
		}
		else if (input.len > 0)
		{
			a = wyr3(input, input.len);
		}
	}
	else
	{
		a = mem::load((ulong*)input.ptr, 1);   // first 8 bytes
		b = mem::load((ulong*)&input[^8], 1);   // a walking 8-byte window based on input.len
	}

	uint128 r = ((uint128)a ^ 0xe703_7ed1_a0b4_28db) * ((uint128)b ^ seed);
	ulong pre_res = (ulong)r ^ (ulong)(r >> 64);

	r = ((uint128)0xe703_7ed1_a0b4_28db ^ input.len) * (uint128)pre_res;
	return (ulong)r ^ (ulong)(r >> 64);
}
