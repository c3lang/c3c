// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::hash::adler32;
import std::core::mem, std::math;

const uint ADLER32_CONST @private = 65521;

<*
 Largest block length for which a and b cannot overflow 32 bits before the modulo
 (255 * n * (n + 1) / 2 + (n + 1) * (ADLER32_CONST - 1) <= 2^32 - 1). A multiple of 16.
*>
const sz NMAX @private = 5552;


struct Adler32
{
	uint a;
	uint b;
}

fn void Adler32.init(&self)
{
	*self = { 1, 0 };
}

fn void Adler32.update_byte(&self, char c)
{
	self.a = (self.a + c) % ADLER32_CONST;
	self.b = (self.b + self.a) % ADLER32_CONST;
}

<*
 Adds `data` to the checksum.

 The data is processed in blocks of at most NMAX bytes, so the modulo is applied
 once per block instead of once per byte. Full 16 byte groups use portable vectors.
*>
fn void Adler32.update(&self, char[] data)
{
	uint a = self.a;
	uint b = self.b;
	char* p = data.ptr;
	sz n = data.len;

	// 16 byte weights for the vector path: the first byte of a group is counted 16 times, the last once.
	const ushort[<8>] WEIGHTS_LO = { 16, 15, 14, 13, 12, 11, 10, 9 };
	const ushort[<8>] WEIGHTS_HI = { 8, 7, 6, 5, 4, 3, 2, 1 };
	const uint[<4>] LOW_MASK = { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF };

	while (n > 0)
	{
		sz block = n < NMAX ? n : NMAX;
		n -= block;
		sz groups = block / 16;
		if (groups > 0)
		{
			// For group k with running sum S_k (sum of all earlier bytes in this block):
			//   b += 16 * (a_start + S_k) + weighted_sum(group k)
			// so only three vector accumulators are needed and no horizontal sum inside the loop.
			uint[<4>] sum;
			uint[<4>] weighted;
			uint[<4>] prefix;
			for (sz i = 0; i < groups; i++)
			{
				char[<8>] lo = mem::load((char[<8>]*)p, 1);
				char[<8>] hi = mem::load((char[<8>]*)(p + 8), 1);
				p += 16;
				prefix += sum;
				ushort[<8>] xl = (ushort[<8>])lo;
				ushort[<8>] xh = (ushort[<8>])hi;
				uint[<4>] s = bitcast(xl + xh, uint[<4>]);
				uint[<4>] w = bitcast(xl * WEIGHTS_LO + xh * WEIGHTS_HI, uint[<4>]);
				sum += (s & LOW_MASK) + (s >> 16);
				weighted += (w & LOW_MASK) + (w >> 16);
			}
			// Stays below 2^32 for the same reason NMAX does.
			b += (uint)groups * 16 * a + 16 * prefix.sum() + weighted.sum();
			a += sum.sum();
		}
		for (sz i = groups * 16; i < block; i++)
		{
			a += *p++;
			b += a;
		}
		a %= ADLER32_CONST;
		b %= ADLER32_CONST;
	}

	*self = { a, b };
}

fn uint Adler32.final(&self)
{
	return (self.b << 16) | self.a;
}

fn uint hash(char[] data)
{
	Adler32 adler;
	adler.init();

	adler.update(data);

	return adler.final();
}