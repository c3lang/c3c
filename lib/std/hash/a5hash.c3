// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// An implementation of Aleksey Vaneev's a5hash, version 5.16, in C3:
//   https://github.com/avaneev/komihash
//
// The license for komihash from the above repository at the time of writing is as follows:
//
// >> MIT License
// >>
// >> Copyright (c) 2025 Aleksey Vaneev
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
module std::hash::a5hash;


macro @a5mul(#u, #v, #lo, #hi) @local
{
	uint128 imd = (uint128)#u * (uint128)#v;
	#lo = (ulong)imd;
	#hi = (ulong)(imd >> 64);
}


fn ulong hash(char[] data, ulong seed = 0)
{
	ulong seed1 = 0x243F_6A88_85A3_08D3 ^ data.len;
	ulong seed2 = 0x4528_21E6_38D0_1377 ^ data.len;
	ulong val10 = 0xAAAA_AAAA_AAAA_AAAA;
	ulong val01 = 0x5555_5555_5555_5555;
	ulong a, b;

	@a5mul(seed2 ^ (seed & val10), seed1 ^ (seed & val01), seed1, seed2);

	val10 ^= seed2;

	if (@likely(data.len > 3))
	{
		if (data.len > 16)
		{
			val01 ^= seed1;

			for (; data.len > 16; data = data[16..])
			{
				@a5mul(
					@unaligned_load(((ulong*)data.ptr)[0], 1) ^ seed1,
					@unaligned_load(((ulong*)data.ptr)[1], 1) ^ seed2,
					seed1, seed2
				);

				seed1 += val01;
				seed2 += val10;
			}

			a = @unaligned_load(*(ulong*)(data.ptr + (uptr)data.len - 16), 1);
			b = @unaligned_load(*(ulong*)(data.ptr + (uptr)data.len - 8), 1);
		}
		else
		{
			usz mo = data.len >> 3;

			a = ((ulong)@unaligned_load(*(uint*)data.ptr, 1) << 32)
				| @unaligned_load(*(uint*)(data.ptr + data.len - 4), 1);

			b = ((ulong)@unaligned_load(*(uint*)(data.ptr + mo * 4), 1) << 32)
				| @unaligned_load(*(uint*)(data.ptr + data.len - 4 - (mo * 4)), 1);
		}

		@a5mul(a ^ seed1, b ^ seed2, seed1, seed2);
		@a5mul(val01 ^ seed1, seed2, a, b);

		return a ^ b;
	}

	a = b = 0;

	if (data.len != 0)
	{
		a = (ulong)data[0];
		a = data[0];

		if (data.len != 1)
		{
			a |= (ulong)data[1] << 8;

			if (data.len != 2)
			{
				a |= (ulong)data[2] << 16;
			}
		}
	}

	@a5mul(a ^ seed1, b ^ seed2, seed1, seed2);
	@a5mul(val01 ^ seed1, seed2, a, b);

	return a ^ b;
}
