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
const  NONCE_SIZE = 12;

<* A required ChaCha20 "magic" value used for state initialization. *>
const char[] MAGIC = "expand 32-byte k";

<* Once a single ChaCha20 context has processed this many bytes, a new nonce MUST be used, unless the static `permit_overflow` runtime module variable is set to true. *>
const CHACHA20_NONCE_REUSE_LIMIT = 64 * (1ull << 32);

<* SECURITY WARNING: This boolean should always remain 'false'. If set to 'true', you accept the security implications of nonce re-use in ChaCha20 encryption operations. *>
bool permit_overflow = false;


<* A context structure used to track an ongoing ChaCha20 transformation. *>
struct ChaCha20
{
	<* The position within a block before permuting the rounds. *>
	usz position;
	<* Count of bytes processed. Useful to track an approach to the 256GiB limit of a single context. *>
	ulong bytes_processed;
	<* The key stream or state used during cipher block operations. *>
	uint[16] key_stream;
	<* The secret key for the context. *>
	char[32] key;
	<* The one-time nonce (or IV - initialization vector) used for the context. *>
	char[12] nonce;
	<* Internal state of the cipher. *>
	uint[16] state;
}


<* The meat and potatoes of the ChaCha20 stream cipher. *>
macro quarter_round(uint* x, int a, int b, int c, int d) @local
{
	x[a] += x[b]; x[d] = (x[d] ^ x[a]).rotl(16);
	x[c] += x[d]; x[b] = (x[b] ^ x[c]).rotl(12);
	x[a] += x[b]; x[d] = (x[d] ^ x[a]).rotl(8);
	x[c] += x[d]; x[b] = (x[b] ^ x[c]).rotl(7);
}


<* Process the next (or final) chunk of ingested data. *>
fn void ChaCha20.next_block(&self) @local
{
	int i;

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
	++counter[0];
}


<* Initialize a ChaCha20 transformation context. *>
fn void ChaCha20.init(&self, char[] key, char[] nonce, uint counter = 1)
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
	self.state[12] = counter;
	self.state[13] = @unaligned_load(*(uint*)&nonce[0 * 4], 1);
	self.state[14] = @unaligned_load(*(uint*)&nonce[1 * 4], 1);
	self.state[15] = @unaligned_load(*(uint*)&nonce[2 * 4], 1);
	self.bytes_processed = 0;
	self.position = 64;
}


<* Transform some input data using the current context structure. *>
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

	self.bytes_processed += data.len;
	if (@unlikely(self.bytes_processed >= CHACHA20_NONCE_REUSE_LIMIT && !permit_overflow))
	{
		abort(
			"ChaCha20 transform limit (~256 GiB) exceeded. You can set 'chacha20::permit_overflow = true;' at"
			" runtime to disable this panic, but you accept the terrible SECURITY IMPLICATIONS of doing so."
		);
	}
}


<* Destroy the current context structure by zeroing all fields. *>
fn void ChaCha20.destroy(&self) => *self = {};


<*
 Perform an in-place transformation of some data in a buffer, without cloning the data to a new buffer.

 @param[inout] data : `The data to transform (encrypt or decrypt).`
 @param[in] key : `The secret key used for the transformation operation.`
 @param[in] nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn void crypt(char[] data, char[] key, char[] nonce, uint counter = 1) @private
{
	if (@unlikely(!data.len)) return;
	ChaCha20 c @noinit;
	defer c.destroy();
	c.init(key, nonce, counter);
	c.transform(data);
}
alias encrypt_mut = crypt;
alias decrypt_mut = crypt;


<*
 Perform a transformation of some data cloned from a source buffer.

 @param[&inout] allocator : `The memory allocator which controls allocation of the cloned input data.`
 @param[inout] data : `The data to transform (encrypt or decrypt).`
 @param[in] key : `The secret key used for the transformation operation.`
 @param[in] nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn char[] crypt_clone(Allocator allocator, char[] data, char[] key, char[] nonce, uint counter = 1) @private
{
	if (@unlikely(!data.len)) return {};
	char[] buff = allocator::clone_slice(allocator, data);
	crypt(buff, key, nonce, counter);
	return buff;
}
alias encrypt = crypt_clone;
alias decrypt = crypt_clone;


<*
 Perform a transformation of some data cloned from a source buffer by the temp allocator.

 @param[inout] data : `The data to transform (encrypt or decrypt).`
 @param[in] key : `The secret key used for the transformation operation.`
 @param[in] nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn char[] tcrypt_clone(char[] data, char[] key, char[] nonce, uint counter = 1) @private
{
	return crypt_clone(tmem, data, key, nonce, counter);
}
alias tencrypt = tcrypt_clone;
alias tdecrypt = tcrypt_clone;
