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
const NONCE_SIZE = 12;

<* A required ChaCha20 "magic" value used for state initialization. *>
const char[] MAGIC = "expand 32-byte k";

<*
 Once a single ChaCha20 context has processed this many bytes, a new nonce MUST be used,
 unless the static `permit_overflow` runtime module variable is set to true.
*>
const CHACHA20_NONCE_REUSE_LIMIT = 64 * (1ull << 32);

<*
 SECURITY WARNING:
 This boolean should always remain 'false'. If set to 'true', you accept the security
 implications of nonce re-use caused by an overflow in the cipher's  'counter' field.

 This security warning is only applicable when a single ChaCha20 context is being used
 to process more than about 256 GiB of data.
*>
bool permit_overflow = false;


<* A context structure used to track an ongoing ChaCha20 transformation. *>
struct ChaCha20
{
	<* The position within a block before permuting the rounds. *>
	usz position;
	<* Count of bytes processed. Useful to track an approach to the 256GiB limit of a single context. *>
	ulong bytes_processed;
	<* The key stream or state used during cipher block operations. *>
	uint[16] key_stream @align(ulong.sizeof);
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
fn void ChaCha20.mutate_keystream(&self) @local @inline
{
	self.key_stream[..] = self.state[..];

	for (usz i = 0; i < 10; i++) // unrolling this does not improve performance measurably
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

	// NOTE: This would 'feel' like a performance hit, but testing the benchmark doesn't show any noticeable
	//   difference on -O5 between this and a for-loop, or even an unrolled loop with compile-time '$for'.
	array::@zip_into(self.key_stream[..], self.state[..], fn (a, b) => a + b);

	self.state[12]++; // increment the block counter (rollovers are ok)
}

<*
 Initialize a ChaCha20 transformation context.

 @param key : `The secret key used for the transformation operation.`
 @param nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn void ChaCha20.init(&self, char[KEY_SIZE] key, char[NONCE_SIZE] nonce, uint counter = 0)
{
	// Init block.
	self.position = BLOCK_SIZE; // start at the "end" of a block on init
	self.bytes_processed = 0;
	self.key[..] = key[..];
	self.nonce[..] = nonce[..];
	((char*)&self.state[0])[:MAGIC.len] = MAGIC[..];
	((char*)&self.state[4])[:KEY_SIZE] = key[..];
	self.state[12] = counter;
	((char*)&self.state[13])[:NONCE_SIZE] = nonce[..];
}

<*
 Transform some input data using the current context structure.

 @param[inout] data : `The data to transform (encrypt or decrypt).`
*>
fn void ChaCha20.transform(&self, char[] data)
{
	if (!data.len) return;

	usz original_length = data.len;
	char[] key_stream = @as_char_view(self.key_stream);

	// 1. Process remaining bytes in the current keystream block.
	if (self.position < BLOCK_SIZE)
	{
		usz len = data.len < (BLOCK_SIZE - self.position) ? data.len : (BLOCK_SIZE - self.position);
		for (usz i = 0; i < len; i++) data[i] ^= key_stream[self.position + i];
		self.position += len;
		data = data[len..];
	}

	// 2. Get the amount of bytes offset from the nearest alignment boundary.
	//   Process full blocks at a time, word by word according to the system's architecture.
	//   Any extra bytes on each side are dynamically processed byte-by-byte.
    usz offset = usz.sizeof - (((usz)data.ptr % usz.sizeof) ?: usz.sizeof);

	for (usz x = offset; data.len >= BLOCK_SIZE; data = data[BLOCK_SIZE..], x = offset)
	{
		self.mutate_keystream();
        if (offset) foreach (i, &b : data[:offset]) *b ^= key_stream[i];
        char[] aligned_data = data[offset..];
		for (; x <= (BLOCK_SIZE - usz.sizeof); x += usz.sizeof)
		{
			((usz*)aligned_data.ptr)[x / usz.sizeof] ^= @unaligned_load(*(usz*)(&key_stream[x]), 1);
		}
        for (; x < BLOCK_SIZE; x++) data[x] ^= key_stream[x];
	}

	// 3. Process any remaining bytes.
	if (data.len > 0)
	{
		self.mutate_keystream();
		for (usz i = 0; i < data.len; i++) data[i] ^= key_stream[i];
		self.position = data.len;
	}

	// All done. Capture the transformed length of data and check limits.
	self.bytes_processed += original_length;
	if (@unlikely(self.bytes_processed >= CHACHA20_NONCE_REUSE_LIMIT && !permit_overflow))
	{
		abort(
			"ChaCha20 transform limit (~256 GiB) exceeded. You can set 'chacha20::permit_overflow = true;' at"
			" runtime to disable this panic, but you accept the terrible SECURITY IMPLICATIONS of doing so."
		);
	}
}

<* Destroy the current context structure by zeroing all fields. *>
fn void ChaCha20.destroy(&self) => mem::zero_volatile(@as_char_view(*self));


<*
 Perform an in-place transformation of some data in a buffer, without cloning the data to a new buffer.

 @param[inout] data : `The data to transform (encrypt or decrypt).`
 @param key : `The secret key used for the transformation operation.`
 @param nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn void crypt(char[] data, char[KEY_SIZE] key, char[NONCE_SIZE] nonce, uint counter = 0) @private
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
 @param key : `The secret key used for the transformation operation.`
 @param nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn char[] crypt_clone(Allocator allocator, char[] data, char[KEY_SIZE] key, char[NONCE_SIZE] nonce, uint counter = 0) @private
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
 @param key : `The secret key used for the transformation operation.`
 @param nonce : `The one-time nonce to use for the transformation operation.`
 @param counter : `An optional counter value to adjust the stream's position.`

 @require key.len == KEY_SIZE : `Input key slice is not the correct length (32 bytes).`
 @require nonce.len ==  NONCE_SIZE : `Input nonce slice is not the correct length (12 bytes).`
*>
fn char[] tcrypt_clone(char[] data, char[KEY_SIZE] key, char[NONCE_SIZE] nonce, uint counter = 0) @private
{
	return crypt_clone(tmem, data, key, nonce, counter);
}
alias tencrypt = tcrypt_clone;
alias tdecrypt = tcrypt_clone;
