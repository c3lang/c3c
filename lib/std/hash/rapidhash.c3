/*
 * rapidhash V3 - Very fast, high quality, platform-independent hashing algorithm.
 *
 * Based on 'wyhash', by Wang Yi <godspeed_china@yeah.net>
 * 
 * Copyright (C) 2025 Nicolas De Carli
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * You can contact the author at:
 *   - rapidhash source repository: https://github.com/Nicoshev/rapidhash
 */
module std::hash::rapidhash;


const ulong[8] SECRET @local = {
	0x2d358dccaa6c78a5,
	0x8bb84b93962eacc9,
	0x4b33a62ed433d4a3,
	0x4d5a2da51de1aa47,
	0xa0761d6478bd642f,
	0xe7037ed1a0b428db,
	0x90ed1765281c388c,
	0xaaaaaaaaaaaaaaaa,
};


macro void @mum(#a, #b) @local
{
	uint128 r = (uint128)#a * (uint128)#b;
	#a = (ulong)r;
	#b = (ulong)(r >> 64);
}


fn ulong mix(ulong a, ulong b) @inline @local
{
	@mum(a, b);
	return a ^ b;
}


// Reads for inputs of at most 16 bytes, which all variants share.
macro void @read_short(char* p, usz len, #seed, #a, #b) @local
{
	if (len >= 4)
	{
		#seed ^= (ulong)len;
		if (len >= 8)
		{
			#a = bitorder::load_le((ulong*)p);
			#b = bitorder::load_le((ulong*)(p + len - 8));
		}
		else
		{
			#a = bitorder::load_le((uint*)p);
			#b = bitorder::load_le((uint*)(p + len - 4));
		}
	}
	else if (len > 0)
	{
		#a = ((ulong)p[0] << 45) | p[len - 1];
		#b = p[len >> 1];
	}
}


fn ulong finish(ulong a, ulong b, ulong seed, usz i) @inline @local
{
	a ^= SECRET[1];
	b ^= seed;
	@mum(a, b);
	return mix(a ^ SECRET[7], b ^ SECRET[1] ^ (ulong)i);
}


<*
 Hash data with rapidhash, the general purpose variant.

 @param data : "The data to hash"
 @param seed : "The seed, which alters the result predictably"
 @return "The 64-bit hash"
*>
fn ulong hash(char[] data, ulong seed = 0)
{
	char* p = data.ptr;
	usz i = data.len;
	ulong a;
	ulong b;

	seed ^= mix(seed ^ SECRET[2], SECRET[1]);

	if (@likely(i <= 16))
	{
		@read_short(p, i, seed, a, b);
		return finish(a, b, seed, i);
	}

	if (i > 112)
	{
		ulong see1 = seed;
		ulong see2 = seed;
		ulong see3 = seed;
		ulong see4 = seed;
		ulong see5 = seed;
		ulong see6 = seed;
		do
		{
			seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[0], bitorder::load_le((ulong*)(p + 8)) ^ seed);
			see1 = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 24)) ^ see1);
			see2 = mix(bitorder::load_le((ulong*)(p + 32)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 40)) ^ see2);
			see3 = mix(bitorder::load_le((ulong*)(p + 48)) ^ SECRET[3], bitorder::load_le((ulong*)(p + 56)) ^ see3);
			see4 = mix(bitorder::load_le((ulong*)(p + 64)) ^ SECRET[4], bitorder::load_le((ulong*)(p + 72)) ^ see4);
			see5 = mix(bitorder::load_le((ulong*)(p + 80)) ^ SECRET[5], bitorder::load_le((ulong*)(p + 88)) ^ see5);
			see6 = mix(bitorder::load_le((ulong*)(p + 96)) ^ SECRET[6], bitorder::load_le((ulong*)(p + 104)) ^ see6);
			p += 112;
			i -= 112;
		} while (i > 112);
		seed ^= see1;
		see2 ^= see3;
		see4 ^= see5;
		seed ^= see6;
		see2 ^= see4;
		seed ^= see2;
	}

	if (i > 16)
	{
		seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[2], bitorder::load_le((ulong*)(p + 8)) ^ seed);
		if (i > 32)
		{
			seed = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 24)) ^ seed);
			if (i > 48)
			{
				seed = mix(bitorder::load_le((ulong*)(p + 32)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 40)) ^ seed);
				if (i > 64)
				{
					seed = mix(bitorder::load_le((ulong*)(p + 48)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 56)) ^ seed);
					if (i > 80)
					{
						seed = mix(bitorder::load_le((ulong*)(p + 64)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 72)) ^ seed);
						if (i > 96)
						{
							seed = mix(bitorder::load_le((ulong*)(p + 80)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 88)) ^ seed);
						}
					}
				}
			}
		}
	}

	// These may step back before the current p, which is safe since the input is over 16 bytes.
	a = bitorder::load_le((ulong*)(p + i - 16)) ^ (ulong)i;
	b = bitorder::load_le((ulong*)(p + i - 8));
	return finish(a, b, seed, i);
}


<*
 Hash data with rapidhashMicro, for cases where cache misses matter. It produces different
 results from `hash`, is faster for inputs up to about 512 bytes and slower above that.

 @param data : "The data to hash"
 @param seed : "The seed, which alters the result predictably"
 @return "The 64-bit hash"
*>
fn ulong micro(char[] data, ulong seed = 0)
{
	char* p = data.ptr;
	usz i = data.len;
	ulong a;
	ulong b;

	seed ^= mix(seed ^ SECRET[2], SECRET[1]);

	if (@likely(i <= 16))
	{
		@read_short(p, i, seed, a, b);
		return finish(a, b, seed, i);
	}

	if (i > 80)
	{
		ulong see1 = seed;
		ulong see2 = seed;
		ulong see3 = seed;
		ulong see4 = seed;
		do
		{
			seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[0], bitorder::load_le((ulong*)(p + 8)) ^ seed);
			see1 = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 24)) ^ see1);
			see2 = mix(bitorder::load_le((ulong*)(p + 32)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 40)) ^ see2);
			see3 = mix(bitorder::load_le((ulong*)(p + 48)) ^ SECRET[3], bitorder::load_le((ulong*)(p + 56)) ^ see3);
			see4 = mix(bitorder::load_le((ulong*)(p + 64)) ^ SECRET[4], bitorder::load_le((ulong*)(p + 72)) ^ see4);
			p += 80;
			i -= 80;
		} while (i > 80);
		seed ^= see1;
		see2 ^= see3;
		seed ^= see4;
		seed ^= see2;
	}

	if (i > 16)
	{
		seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[2], bitorder::load_le((ulong*)(p + 8)) ^ seed);
		if (i > 32)
		{
			seed = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 24)) ^ seed);
			if (i > 48)
			{
				seed = mix(bitorder::load_le((ulong*)(p + 32)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 40)) ^ seed);
				if (i > 64)
				{
					seed = mix(bitorder::load_le((ulong*)(p + 48)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 56)) ^ seed);
				}
			}
		}
	}

	// These may step back before the current p, which is safe since the input is over 16 bytes.
	a = bitorder::load_le((ulong*)(p + i - 16)) ^ (ulong)i;
	b = bitorder::load_le((ulong*)(p + i - 8));
	return finish(a, b, seed, i);
}


<*
 Hash data with rapidhashNano, for cases where small code size matters. It produces different
 results from `hash`, is fastest for inputs up to 48 bytes and may be much slower above that.

 @param data : "The data to hash"
 @param seed : "The seed, which alters the result predictably"
 @return "The 64-bit hash"
*>
fn ulong nano(char[] data, ulong seed = 0)
{
	char* p = data.ptr;
	usz i = data.len;
	ulong a;
	ulong b;

	seed ^= mix(seed ^ SECRET[2], SECRET[1]);

	if (@likely(i <= 16))
	{
		@read_short(p, i, seed, a, b);
		return finish(a, b, seed, i);
	}

	if (i > 48)
	{
		ulong see1 = seed;
		ulong see2 = seed;
		do
		{
			seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[0], bitorder::load_le((ulong*)(p + 8)) ^ seed);
			see1 = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[1], bitorder::load_le((ulong*)(p + 24)) ^ see1);
			see2 = mix(bitorder::load_le((ulong*)(p + 32)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 40)) ^ see2);
			p += 48;
			i -= 48;
		} while (i > 48);
		seed ^= see1;
		seed ^= see2;
	}

	if (i > 16)
	{
		seed = mix(bitorder::load_le((ulong*)p) ^ SECRET[2], bitorder::load_le((ulong*)(p + 8)) ^ seed);
		if (i > 32)
		{
			seed = mix(bitorder::load_le((ulong*)(p + 16)) ^ SECRET[2], bitorder::load_le((ulong*)(p + 24)) ^ seed);
		}
	}

	// These may step back before the current p, which is safe since the input is over 16 bytes.
	a = bitorder::load_le((ulong*)(p + i - 16)) ^ (ulong)i;
	b = bitorder::load_le((ulong*)(p + i - 8));
	return finish(a, b, seed, i);
}
