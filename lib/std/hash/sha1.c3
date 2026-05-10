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
	while ((self.count[0] & 504u) != 448u)
	{
		self.update((char[]){ 0 });
	}

	self.update(&finalcount);
	char[HASH_BYTES] digest;
	for (int i = 0; i < HASH_BYTES; i++)
	{
		digest[i] = (char)((self.state[i >> 2] >> ((3 - (i & 3)) * 8)) & 0xFF);
	}

	// Clear mem
	*self = {};
	finalcount = {};
	return digest;
}

union Long16 @local
{
	char[BLOCK_BYTES] c;
	uint[16] l;
}

macro uint blk(Long16* block, i) @local
{
	return (block.l[i & 15u] = (block.l[(i + 13u) & 15u] ^ block.l[(i + 8u) & 15u]
		^ block.l[(i + 2u) & 15u] ^ block.l[i & 15u]).rotl(1u));
}

macro uint blk0(Long16* block, i) @local
{
	$if env::BIG_ENDIAN:
		return block.l[i];
	$else
		return block.l[i] = (block.l[i].rotl(24u) & 0xFF00FF00) | (block.l[i].rotl(8u) & 0x00FF00FF);
	$endif
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

	for (uint i = 0; i < 16; i++) {
		uint t = a.rotl(5) + ((b & (c ^ d)) ^ d) + e + 0x5A827999 + blk0(&block, i);
		e = d; d = c; c = b.rotl(30); b = a; a = t;
	}
	for (uint i = 16; i < 20; i++) {
		uint t = a.rotl(5) + ((b & (c ^ d)) ^ d) + e + 0x5A827999 + blk(&block, i);
		e = d; d = c; c = b.rotl(30); b = a; a = t;
	}
	for (uint i = 20; i < 40; i++) {
		uint t = a.rotl(5) + (b ^ c ^ d) + e + 0x6ED9EBA1 + blk(&block, i);
		e = d; d = c; c = b.rotl(30); b = a; a = t;
	}
	for (uint i = 40; i < 60; i++) {
		uint t = a.rotl(5) + ((b & c) | (d & (b | c))) + e + 0x8F1BBCDC + blk(&block, i);
		e = d; d = c; c = b.rotl(30); b = a; a = t;
	}
	for (uint i = 60; i < 80; i++) {
		uint t = a.rotl(5) + (b ^ c ^ d) + e + 0xCA62C1D6 + blk(&block, i);
		e = d; d = c; c = b.rotl(30); b = a; a = t;
	}

	(*state)[0] += a; (*state)[1] += b; (*state)[2] += c;
	(*state)[3] += d; (*state)[4] += e;
	a = b = c = d = e = 0;
	block = {};
}