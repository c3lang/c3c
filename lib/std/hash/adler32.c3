// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::hash::adler32;

const uint ADLER32_CONST @private = 65521;


struct Adler32
{
	uint a;
	uint b;
}

fn void Adler32.init(&self)
{
	*self = { 1, 0 };
}

fn void Adler32.updatec(&self, char c)
{
	self.a = (self.a + c) % ADLER32_CONST;
	self.b = (self.b + self.a) % ADLER32_CONST;
}

fn void Adler32.update(&self, char[] data)
{
	// Safe chunking constant which is optimized for L1 cache on most systems 32768 (32 KB).
	// 0x8000 ~ (2^32 / 65521 / 2).
	// The division is done so that we are guarenteed to never overflow.
	const uint SAFE_CHUNKING_SIZE = 0x8000;

	// In order
	const uint UNROLL_SIZE = 8;

	uint a = self.a;
	uint b = self.b;

	char* buf = data;
	usz len = data.len;

	// Align pointer traversing buffer pointer to the unroll alignment size.
	if (len % UNROLL_SIZE != 0)
	{
		do
		{
			a += *buf;
			b += a;

			buf++;
			len--;
		} while (len % UNROLL_SIZE != 0);

		if (a >= ADLER32_CONST)
		{
			a -= ADLER32_CONST;
		}

		b %= ADLER32_CONST;
	}

	// Calculate rest of adler32 checksum.
	while (len > 0)
	{
		$for var $i = 0; $i < UNROLL_SIZE; $i++:
			a += buf[$i]; b += a;
		$endfor

		len -= UNROLL_SIZE;
		buf += UNROLL_SIZE;

		// Even with 8 max value (0xFF) bytes being additioned to a (0xFF * 8 = 2040 for worst case).
		// There is no chance that a will be > 2 * ADLER32_CONST, so modulo is not needed here.
		// So its more performant to use subtraction.
		if (a >= ADLER32_CONST)
		{
			a -= ADLER32_CONST;
		}

		// We need to periodically chunk b because it accumulates a which is a sum, so it grows rapidly.
		// So every 4K of bytes we modulo in order to prevent uint integer overflow.
		if (len % SAFE_CHUNKING_SIZE == 0)
		{
			b %= ADLER32_CONST;
		}
	}

	// No need to explicitely modulo after loop end with ADLER32_CONST.
	// As a and b are guarenteed to be under ADLER32_CONST.

	// Do assert on debug.
	assert(a < ADLER32_CONST);
	assert(b < ADLER32_CONST);

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