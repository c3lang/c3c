// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// An implementation of Aleksey Vaneev's komihash, version 5.27, in C3:
//   https://github.com/avaneev/komihash
//
// The license for komihash from the above repository at the time of writing is as follows:
//
// >> MIT License
// >>
// >> Copyright (c) 2021-2025 Aleksey Vaneev
// >>
// >> Permission is hereby granted, free of charge, to any person obtaining a copy
// >> of this software and associated documentation files (the "Software"), to deal
// >> in the Software without restriction, including without limitation the rights
// >> to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// >> copies of the Software, and to permit persons to whom the Software is
// >> furnished to do so, subject to the following conditions:
// >>
// >> The above copyright notice and this permission notice shall be included in all
// >> copies or substantial portions of the Software.
// >>
// >> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// >> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// >> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// >> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// >> LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// >> OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// >> SOFTWARE.
//
//
module std::hash::komi;


macro void @komimul(#u, #v, #lo, #hi) @local
{
	uint128 imd = (uint128)#u * (uint128)#v;
	#lo = (ulong)imd;
	#hi += (ulong)(imd >> 64);
}


fn ulong hash(char[] data, ulong seed = 0)
{
	ulong seed1 = 0x243F_6A88_85A3_08D3 ^ (seed & 0x5555_5555_5555_5555);
	ulong seed5 = 0x4528_21E6_38D0_1377 ^ (seed & 0xAAAA_AAAA_AAAA_AAAA);
	ulong r1h, r2h;

	// HASHROUND
	@komimul(seed1, seed5, seed1, seed5);
	seed1 ^= seed5;

	if (@likely(data.len < 16))
	{
		r1h = seed1;
		r2h = seed5;

		if (@likely(data.len >= 8))
		{
			r1h ^= @unaligned_load(*(ulong*)data.ptr, 1);

			r2h ^= (data.len < 12)
				? ((data[data.len - 3] | ((ulong)data[data.len - 2] << 8) | ((ulong)data[data.len - 1] << 16) | ((ulong)1 << 24)) >> ((data.len * 8) ^ 88))
				: (((@unaligned_load(*(uint*)&data[^4], 1) | ((ulong)1 << 32)) >> (128 - data.len * 8)) << 32 | @unaligned_load(*(uint*)&data[8], 1));
		}
		else if (data.len != 0)
		{
			r1h ^= (data.len < 4)
				? (((ulong)1 << (data.len * 8)) ^ data[0] ^ (data.len > 1 ? (ulong)data[1] << 8 : 0) ^ (data.len > 2 ? (ulong)data[2] << 16 : 0))
				: (((@unaligned_load(*(uint*)&data[^4], 1) | ((ulong)1 << 32)) >> (64 - data.len * 8)) << 32 | @unaligned_load(*(uint*)&data[0], 1));
		}
	}
	else if (data.len < 32)
	{
		// HASH16
		@komimul(
			@unaligned_load(*(ulong*)&data[0], 1) ^ seed1,
			@unaligned_load(*(ulong*)&data[8], 1) ^ seed5,
			seed1, seed5
		);
		seed1 ^= seed5;

		if (data.len < 24)
		{
			r1h = (((@unaligned_load(*(ulong*)&data[^8], 1) >> 8) | ((ulong)1 << 56)) >> (((int)(data.len * 8) ^ 184))) ^ seed1;
			r2h = seed5;
		}
		else
		{
			r1h = @unaligned_load(*(ulong*)&data[16], 1) ^ seed1;
			r2h = (((@unaligned_load(*(ulong*)&data[^8], 1) >> 8) | ((ulong)1 << 56)) >> (((int)(data.len * 8) ^ 248))) ^ seed5;
		}
	}
	else
	{
		if (data.len >= 64)
		{
			ulong[8] seeds = {
				seed1, 0x1319_8A2E_0370_7344 ^ seed1, 0xA409_3822_299F_31D0 ^ seed1, 0x082E_FA98_EC4E_6C89 ^ seed1,
				seed5, 0xBE54_66CF_34E9_0C6C ^ seed5, 0xC0AC_29B7_C97C_50DD ^ seed5, 0x3F84_D5B5_B547_0917 ^ seed5,
			};

			// HASHLOOP64
			for (; data.len >= 64; data = data[64:^64])
			{
				$for var $x = 0; $x < 4; ++$x :
					@komimul(
						@unaligned_load(*(ulong*)&data[0 + ($x * 8)], 1) ^ seeds[$x],
						@unaligned_load(*(ulong*)&data[32 + ($x * 8)], 1) ^ seeds[4 + $x],
						seeds[$x], seeds[4 + $x]
					);
				$endfor

				seeds[3] ^= seeds[6];
				seeds[0] ^= seeds[7];
				seeds[2] ^= seeds[5];
				seeds[1] ^= seeds[4];
			}

			seed1 = seeds[0] ^ seeds[1] ^ seeds[2] ^ seeds[3];
			seed5 = seeds[4] ^ seeds[5] ^ seeds[6] ^ seeds[7];
		}

		for (; data.len >= 16; data = data[16:^16])
		{
			@komimul(
				@unaligned_load(*(ulong*)&data[0], 1) ^ seed1,
				@unaligned_load(*(ulong*)&data[8], 1) ^ seed5,
				seed1, seed5
			);
			seed1 ^= seed5;
		}

		if (data.len < 8)
		{
			// NOTE: This is translated from the original code. It grabs the last ulong off the buffer even though the
			//        data slice is less than 8 bytes. This is possible because this branch only occurs in a loop where
			//        the original data slice length is >= 32.
			r1h = (((@unaligned_load(*(ulong*)(data.ptr + data.len - 8), 1) >> 8) | ((ulong)1 << 56)) >> ((data.len * 8) ^ 0x38)) ^ seed1;
			r2h = seed5;
		}
		else
		{
			r1h = @unaligned_load(*(ulong*)data.ptr, 1) ^ seed1;
			r2h = (((@unaligned_load(*(ulong*)&data[^8], 1) >> 8) | ((ulong)1 << 56)) >> ((data.len * 8) ^ 0x78)) ^ seed5;
		}
	}

	// HASHFIN
	@komimul(r1h, r2h, seed1, seed5);
	seed1 ^= seed5;
	@komimul(seed1, seed5, seed1, seed5);
	seed1 ^= seed5;
	return seed1;
}
