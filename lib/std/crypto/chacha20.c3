// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// ChaCha20 code dedicated from repo: https://github.com/NotsoanoNimus/chacha20_aead.c3l (but massively cleaned)
module std::crypto::chacha20;


<* The typical cipher block size in bytes. *>
const BLOCK_SIZE = 64;

<* Required key size in bytes. *>
const KEY_SIZE = 32;

<* ChaCha20 "nonce" (initialization vector) size. *>
const IV_SIZE = 12;

<* A required ChaCha20 "magic" value used for state initialization. *>
const char[] MAGIC = "expand 32-byte k";


struct ChaCha20
{
    uint[16] key_stream;
    usz position;

    char[32] key;
    char[12] nonce;
    ulong counter;

    uint[16] state;
}


macro quarter_round(uint* x, int a, int b, int c, int d) @local
{
    x[a] += x[b]; x[d] = (x[d] ^ x[a]).rotl(16);
    x[c] += x[d]; x[b] = (x[b] ^ x[c]).rotl(12);
    x[a] += x[b]; x[d] = (x[d] ^ x[a]).rotl(8);
    x[c] += x[d]; x[b] = (x[b] ^ x[c]).rotl(7);
}


fn void ChaCha20.next_block(&self) @local
{
    int i;

    // for (i = 0; i < 16; ++i) self.key_stream[i] = self.state[i];
    self.key_stream[..] = self.state[..];

    for (i = 0; i < 10; ++i) 
	{
		quarter_round(&self.key_stream[0],   0,  4,  8, 12);
		quarter_round(&self.key_stream[0],   1,  5,  9, 13);
		quarter_round(&self.key_stream[0],   2,  6, 10, 14);
		quarter_round(&self.key_stream[0],   3,  7, 11, 15);
		quarter_round(&self.key_stream[0],   0,  5, 10, 15);
		quarter_round(&self.key_stream[0],   1,  6, 11, 12);
		quarter_round(&self.key_stream[0],   2,  7,  8, 13);
		quarter_round(&self.key_stream[0],   3,  4,  9, 14);
	}

    array::@zip_into(self.key_stream[..], self.state[..], fn (a, b) => a + b);

	uint* counter = &self.state[12];
	counter[0]++;

	if (!counter[0]) counter[1]++;
}

fn void ChaCha20.init(&self, char[] key, char[] nonce, ulong counter = 1)
{
    // Init block.
    self.key[..] = key[..];
    self.nonce[..] = nonce[..];
	self.state[0]  = @unaligned_load(*(uint*)&MAGIC[0 * 4], 1);
	self.state[1]  = @unaligned_load(*(uint*)&MAGIC[1 * 4], 1);
	self.state[2]  = @unaligned_load(*(uint*)&MAGIC[2 * 4], 1);
	self.state[3]  = @unaligned_load(*(uint*)&MAGIC[3 * 4], 1);
	self.state[4]  = @unaligned_load(*(uint*)&key[0 * 4], 1);
	self.state[5]  = @unaligned_load(*(uint*)&key[1 * 4], 1);
	self.state[6]  = @unaligned_load(*(uint*)&key[2 * 4], 1);
	self.state[7]  = @unaligned_load(*(uint*)&key[3 * 4], 1);
	self.state[8]  = @unaligned_load(*(uint*)&key[4 * 4], 1);
	self.state[9]  = @unaligned_load(*(uint*)&key[5 * 4], 1);
	self.state[10] = @unaligned_load(*(uint*)&key[6 * 4], 1);
	self.state[11] = @unaligned_load(*(uint*)&key[7 * 4], 1);
	self.state[12] = (uint)counter;
	self.state[13] = @unaligned_load(*(uint*)&nonce[0 * 4], 1) + (uint)(counter >> 32);
	self.state[14] = @unaligned_load(*(uint*)&nonce[1 * 4], 1);
	self.state[15] = @unaligned_load(*(uint*)&nonce[2 * 4], 1);
    self.counter = counter;
    self.position = 64;
}


fn void ChaCha20.transform(&self, char[] data)
{
	for (usz i = 0; i < data.len; ++i)
	{
		if (self.position >= 64) 
		{
			self.next_block();
			self.position = 0;
		}
		data[i] ^= @as_char_view(self.key_stream)[self.position++];
	}
}


fn void ChaCha20.destroy(&self) => *self = {};


fn void crypt(char[] data, char[] key, char[] iv, uint counter = 1) @private
{
    if (!data.len) return;
	ChaCha20 c @noinit;
    defer c.destroy();
	c.init(key, iv, counter);
	c.transform(data);
}

alias encrypt_mut = crypt;
alias decrypt_mut = crypt;


fn char[] crypt_clone(Allocator allocator, char[] data, char[] key, char[] iv, uint counter = 1) @private
{
    if (!data.len) return {};
    char[] buff = allocator::clone_slice(allocator, data);
    crypt(buff, key, iv, counter);
    return buff;
}

alias encrypt = crypt_clone;
alias decrypt = crypt_clone;

fn char[] tcrypt_clone(char[] data, char[] key, char[] iv, uint counter = 1) @private
{
    return crypt_clone(tmem, data, key, iv, counter);
}

alias tencrypt = tcrypt_clone;
alias tdecrypt = tcrypt_clone;
