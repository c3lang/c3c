// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Implementation was off Steve Reid's SHA-1 C implementation

module std::hash::sha1;
import std::hash::hmac;
import std::bits;

const BLOCK_BYTES = 64;
const HASH_BYTES = 20;

struct Sha1
{
	uint[5] state;
	uint[2] count;
	char[BLOCK_BYTES] buffer;
}

alias HmacSha1 = Hmac{Sha1, HASH_BYTES, BLOCK_BYTES};
alias hmac = hmac::hash{Sha1, HASH_BYTES, BLOCK_BYTES};
alias pbkdf2 = hmac::pbkdf2{Sha1, HASH_BYTES, BLOCK_BYTES};

fn char[HASH_BYTES] hash(char[] data)
{
	Sha1 sha1 @noinit;
	sha1.init();
	sha1.update(data);
	return sha1.final();
}

fn void Sha1.init(&self)
{
	// SHA1 initialization constants
	*self = { .state = { 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0 } };
}

<*
 @param [in] data
 @require data.len <= uint::max
*>
fn void Sha1.update(&self, char[] data)
{
	uint j = self.count[0];
	uint len = (uint)data.len;
	if ((self.count[0] += len << 3) < j) self.count[1]++;
	self.count[1] += len >> 29;
	j = (j >> 3) & 63u;
	uint i;
	if (j + len > 63u)
	{
		i = 64u - j;
		self.buffer[j..] = data[:i];
		sha1_transform(&self.state, &self.buffer);
		for (; i + 63u < len; i += 64u)
		{
			sha1_transform(&self.state, &data[i]);
		}
		j = 0;
	}
	self.buffer[j:len - i] = data[i..];
}

fn char[HASH_BYTES] Sha1.final(&self)
{
	char[8] finalcount;
	for (int i = 0; i < 8; i++)
	{
		finalcount[i] = (char)((self.count[(i >= 4 ? 0 : 1)] >> ((3 - (i & 3)) * 8)) & 0xFF);
	}
	self.update((char[]){ 0o200 });

	char[63] padding;
	uint cur_bits = self.count[0] & 504u;
	uint pad_bytes = (cur_bits < 448u ? 448u - cur_bits : 504u + 448u - cur_bits) >> 3;
	self.update(padding[:pad_bytes]);
	self.update(&finalcount);

	char[HASH_BYTES] digest;
	for (int i = 0; i < HASH_BYTES; i++)
	{
		digest[i] = (char)((self.state[i >> 2] >> ((3 - (i & 3)) * 8)) & 0xFF);
	}
	return digest;
}

union Long16 @local
{
	char[BLOCK_BYTES] c;
	uint[16] l;
}

macro uint schedule(Long16* block, uint i) @local
{
	return (block.l[i & 15u] = (block.l[(i + 13u) & 15u] ^ block.l[(i + 8u) & 15u]
		^ block.l[(i + 2u) & 15u] ^ block.l[i & 15u]).rotl(1u));
}
macro uint schedule0(Long16* block, uint i) @local
{
	$if env::BIG_ENDIAN:
		return block.l[i];
	$else
		return block.l[i] = (block.l[i].rotl(24u) & 0xFF00FF00) | (block.l[i].rotl(8u) & 0x00FF00FF);
	$endif
}

macro void r1(uint v, uint* ref, uint x, uint y, uint* z, uint w) @local
{
	var old = *ref;
	*z += ((old & (x ^ y)) ^ y) + w + 0x5A827999 + v.rotl(5);
	*ref = old.rotl(30);
}
macro void r2(uint v, uint* ref, uint x, uint y, uint* z, uint w) @local
{
	var old = *ref;
	*z += (old ^ x ^ y) + w + 0x6ED9EBA1 + v.rotl(5);
	*ref = old.rotl(30);
}
macro void r3(uint v, uint* ref, uint x, uint y, uint* z, uint w) @local
{
	var old = *ref;
	*z += (((old | x) & y) | (old & x)) + w + 0x8F1BBCDC + v.rotl(5);
	*ref = old.rotl(30);
}
macro void r4(uint v, uint* ref, uint x, uint y, uint* z, uint w) @local
{
	var old = *ref;
	*z += (old ^ x ^ y) + w + 0xCA62C1D6 + v.rotl(5);
	*ref = old.rotl(30);
}

<*
 @param [&inout] state
 @param [&in] buffer
*>
fn void sha1_transform(uint[5]* state, char* buffer) @local
{
	Long16 block;
	block.c[..] = buffer[:64];
	uint a = (*state)[0]; uint b = (*state)[1];
	uint c = (*state)[2]; uint d = (*state)[3]; uint e = (*state)[4];

	for (uint i; i < 15; i += 5)
	{
		r1(a, &b, c, d, &e, schedule0(&block, i));
		r1(e, &a, b, c, &d, schedule0(&block, i + 1));
		r1(d, &e, a, b, &c, schedule0(&block, i + 2));
		r1(c, &d, e, a, &b, schedule0(&block, i + 3));
		r1(b, &c, d, e, &a, schedule0(&block, i + 4));
	}
	r1(a, &b, c, d, &e, schedule0(&block, 15));
	r1(e, &a, b, c, &d, schedule(&block, 16));
	r1(d, &e, a, b, &c, schedule(&block, 17));
	r1(c, &d, e, a, &b, schedule(&block, 18));
	r1(b, &c, d, e, &a, schedule(&block, 19));

	for (uint i = 20; i < 40; i += 5)
	{
		r2(a, &b, c, d, &e, schedule(&block, i));
		r2(e, &a, b, c, &d, schedule(&block, i+1));
		r2(d, &e, a, b, &c, schedule(&block, i+2));
		r2(c, &d, e, a, &b, schedule(&block, i+3));
		r2(b, &c, d, e, &a, schedule(&block, i+4));
	}
	for (uint i = 40; i < 60; i += 5)
	{
		r3(a, &b, c, d, &e, schedule(&block, i));
		r3(e, &a, b, c, &d, schedule(&block, i+1));
		r3(d, &e, a, b, &c, schedule(&block, i+2));
		r3(c, &d, e, a, &b, schedule(&block, i+3));
		r3(b, &c, d, e, &a, schedule(&block, i+4));
	}
	for (uint i = 60; i < 80; i += 5)
	{
		r4(a, &b, c, d, &e, schedule(&block, i));
		r4(e, &a, b, c, &d, schedule(&block, i+1));
		r4(d, &e, a, b, &c, schedule(&block, i+2));
		r4(c, &d, e, a, &b, schedule(&block, i+3));
		r4(b, &c, d, e, &a, schedule(&block, i+4));
	}

	(*state)[0] += a; (*state)[1] += b; (*state)[2] += c;
	(*state)[3] += d; (*state)[4] += e;
}
