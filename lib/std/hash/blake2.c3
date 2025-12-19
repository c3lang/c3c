// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// This is based on the original BLAKE2 reference implementation, but it's been
//   significantly changed. You can really see how C3's features shine over the
//   original C source.
//
// You'll note that this may have been better done with generic modules. I'm not
//   an expert, but I'd argue that it's unnecessary for this particular algorithm.
//   There are only two important versions of the algorithm to implement in a "common"
//   (generic) way, and the output size of each type is controlled rather naturally.
//
module std::hash::blake2;


// Common hash output sizes. The library is NOT limited to these six output sizes, but
//   these instead stand in for what would have been "magic numbers" throughout the code.
const SIZE_128 = 16;
const SIZE_160 = 20;
const SIZE_224 = 28;
const SIZE_256 = 32;
const SIZE_384 = 48;
const SIZE_512 = 64;


macro @g(r, m, $s, i, #a, #b, #c, #d) @local
{
	// I really dislike using these conditional one-liners, but... it's the only difference between 2s and 2b besides buffer sizes (mainly).
	#a += #b + m[$s[r][2*i+0]];
	#d = (#d ^ #a).rotr($sizeof(#a) == ulong.sizeof ??? 32 : 16);
	#c += #d;
	#b = (#b ^ #c).rotr($sizeof(#a) == ulong.sizeof ??? 24 : 12);
	#a += #b + m[$s[r][2*i+1]];
	#d = (#d ^ #a).rotr($sizeof(#a) == ulong.sizeof ??? 16 : 8);
	#c += #d;
	#b = (#b ^ #c).rotr($sizeof(#a) == ulong.sizeof ??? 63 : 7);
}

macro @round(r, m, $s, #v) @local
{
	@g(r, m, $s, 0, #v[ 0], #v[ 4], #v[ 8], #v[12]);
	@g(r, m, $s, 1, #v[ 1], #v[ 5], #v[ 9], #v[13]);
	@g(r, m, $s, 2, #v[ 2], #v[ 6], #v[10], #v[14]);
	@g(r, m, $s, 3, #v[ 3], #v[ 7], #v[11], #v[15]);
	@g(r, m, $s, 4, #v[ 0], #v[ 5], #v[10], #v[15]);
	@g(r, m, $s, 5, #v[ 1], #v[ 6], #v[11], #v[12]);
	@g(r, m, $s, 6, #v[ 2], #v[ 7], #v[ 8], #v[13]);
	@g(r, m, $s, 7, #v[ 3], #v[ 4], #v[ 9], #v[14]);
}

macro @common_compress(#instance, $rounds, $iv, $sigma, block) @local
{
	$typeof(#instance.h[0])[16] m, v;

	((char*)&m)[:$sizeof(block)] = block[..];

	v[:8] = #instance.h[..];
	v[ 8] = $iv[0];
	v[ 9] = $iv[1];
	v[10] = $iv[2];
	v[11] = $iv[3];
	v[12] = $iv[4] ^ #instance.t[0];
	v[13] = $iv[5] ^ #instance.t[1];
	v[14] = $iv[6] ^ #instance.f[0];
	v[15] = $iv[7] ^ #instance.f[1];

	$for usz $i = 0; $i < $rounds; $i++:
		@round($i, m, $sigma, v);
	$endfor

	$for usz $i = 0; $i < 8; $i++:
		#instance.h[$i] ^= v[$i] ^ v[$i + 8];
	$endfor
}

macro @add_ctr(#instance, usz amount) @local
{
	#instance.t[0] += ($typeof(#instance.t[0]))amount;
	#instance.t[1] += ($typeof(#instance.t[0]))(#instance.t[0] < amount); // adds 1 on overflow of [0]
}

macro @common_init(#instance, $ParamType, $iv, usz out_len, char[] key = {}, char[] salt = {}, char[] personal = {}) @local
{
	mem::zero_volatile(@as_char_view(*#instance)); // explicitly because habits around hash init usually involve @noinit

	#instance.h[..] = $iv[..];
	#instance.outlen = out_len;

	$ParamType p = {
		.digest_length = (char)out_len,
		.key_length = (char)key.len,
		.fanout = 1,
		.depth = 1,
	};
	if (salt.len) p.salt[:salt.len] = salt[..];
	if (personal.len) p.personal[:personal.len] = personal[..];

	array::@zip_into(((char*)&#instance.h)[:$sizeof(p)], ((char*)&p)[:$sizeof(p)], fn (a, b) => a ^ b); // bytes(self.h) ^= bytes(p)

	if (key.len) 
	{
		char[$sizeof($iv[0])*16] dummy = {};
		dummy[:key.len] = key[..];
		#instance.update(dummy[..]); // consume a FULL block
		mem::zero_volatile(dummy[..]); // do not optimize clearing this from the stack
	}
}

macro @common_update(#instance, $block_size, char[] data) @local
{
	if (@unlikely(!data.len)) return;

	usz fill = $block_size - #instance.buflen;
	if (data.len > fill)
	{
		#instance.buf[#instance.buflen:fill] = data[:fill];
		#instance.buflen = 0;

		@add_ctr(#instance, $block_size);
		#instance.compress(#instance.buf);

		data = data[fill..];

		for (; data.len > $block_size; data = data[$block_size..])
		{
			@add_ctr(#instance, $block_size);
			#instance.compress(data[:$block_size]);
		}
	}
	#instance.buf[#instance.buflen:data.len] = data[..];
	#instance.buflen += data.len;
}

macro @common_final(#instance, $output_length) @local
{
	char[$output_length] result = {};
	if ($output_length != #instance.outlen) return result;

	@add_ctr(#instance, #instance.buflen);
	if (#instance.f[0]) return result; // technically an error return

	if (#instance.last_node) #instance.f[1] = $typeof(#instance.h[0]).max;
	#instance.f[0] = $typeof(#instance.h[0]).max;

	mem::zero_volatile(#instance.buf[#instance.buflen..]); // pad buffer with zeroes
	#instance.compress(#instance.buf);

    defer mem::zero_volatile(@as_char_view(*#instance)); // destroy the current context implicitly

	result[:#instance.outlen] = @as_char_view(#instance.h)[:#instance.outlen];
	return result;
}


// ======================================================================================
// BEGIN Blake2b contents. Do not separate this from Blake2s: there's not really a point.
//
const BLAKE2B_BLOCKBYTES	@local = 128;
const BLAKE2B_OUTBYTES		@local = 64;
const BLAKE2B_KEYBYTES		@local = 64;
const BLAKE2B_SALTBYTES		@local = 16;
const BLAKE2B_PERSONALBYTES	@local = 16;
const ulong[8] BLAKE2B_IV @local = {
    0x6a09e667f3bcc908, 0xbb67ae8584caa73b, 0x3c6ef372fe94f82b, 0xa54ff53a5f1d36f1,
    0x510e527fade682d1, 0x9b05688c2b3e6c1f, 0x1f83d9abfb41bd6b, 0x5be0cd19137e2179
};
const char[16][12] BLAKE2B_SIGMA @local = {
    x'000102030405060708090a0b0c0d0e0f',
    x'0e0a0408090f0d06010c00020b070503',
    x'0b080c0005020f0d0a0e030607010904',
    x'070903010d0c0b0e0206050a04000f08',
    x'0900050702040a0f0e010b0c0608030d',
    x'020c060a000b0803040d07050f0e0109',
    x'0c05010f0e0d040a000706030902080b',
    x'0d0b070e0c01030905000f040806020a',
    x'060f0e090b0300080c020d0701040a05',
    x'0a020804070601050f0b090e030c0d00',
    x'000102030405060708090a0b0c0d0e0f',
    x'0e0a0408090f0d06010c00020b070503',
};

struct Blake2b
{
	ulong[8]					h;
	ulong[2]					t;
	ulong[2]					f;
	char[BLAKE2B_BLOCKBYTES]	buf;
	usz							buflen;
	usz							outlen;
	char						last_node;
}

struct Blake2bParam @packed @local
{
	char						digest_length;	/*  1 */
	char						key_length;		/*  2 */
	char						fanout;			/*  3 */
	char						depth;			/*  4 */
	uint						leaf_length;	/*  8 */
	uint						node_offset;	/* 12 */
	uint						xof_length;		/* 16 */
	char						node_depth;		/* 17 */
	char						inner_length;	/* 18 */
	char[14]					reserved;		/* 32 */
	char[BLAKE2B_SALTBYTES]		salt;			/* 48 */
	char[BLAKE2B_PERSONALBYTES]	personal;		/* 64 */
}

<*
 @require $defined(data[0]) &&& $typeof(data[0]) == char : "Input data must be a char slice, char array, or string value."
*>
macro blake2b_hash($out_len, data, char[] key = {}, char[] salt = {})
{
	Blake2b b @noinit;
    b.init($out_len, key, salt);
	b.update(data[..]);
	return b.final($out_len);
}
alias b = blake2b_hash;

// See RFC 7693, Section 4 for common parameter sets.
macro blake2b_224(data, char[] key = {}, char[] salt = {}) => blake2b_hash(SIZE_224, data, key, salt);
macro blake2b_256(data, char[] key = {}, char[] salt = {}) => blake2b_hash(SIZE_256, data, key, salt);
macro blake2b_384(data, char[] key = {}, char[] salt = {}) => blake2b_hash(SIZE_384, data, key, salt);
macro blake2b_512(data, char[] key = {}, char[] salt = {}) => blake2b_hash(SIZE_512, data, key, salt);

alias b_224 = blake2b_224;
alias b_256 = blake2b_256;
alias b_384 = blake2b_384;
alias b_512 = blake2b_512;

<*
 Blake2b initialization method. Presents various options 

 @param out_len : "The desired output length from the hash function."
 @param[in] key : "An optional key value to use (keys the entire hash value to give HMAC-like functionality)."
 @param[in] salt : "An optional salt value to use when generating the hash."
 @param[in] personal : "An optional personalization value to use when generating the hash."

 @require out_len > 0 && out_len <= BLAKE2B_OUTBYTES : "Blake2 output length must be within the proper range."
 @require !key.ptr || (key.len > 0 && key.len <= BLAKE2B_KEYBYTES) : "A specified key's length must be within the proper range."
 @require !salt.ptr || (salt.len > 0 && salt.len <= BLAKE2B_SALTBYTES) : "A specified salt's length must be within the proper range."
 @require !personal.ptr || (personal.len > 0 && personal.len <= BLAKE2B_PERSONALBYTES) : "A specified personalization's length must be within the proper range."
*>
fn void Blake2b.init(&self, usz out_len, char[] key = {}, char[] salt = {}, char[] personal = {})
	=> @common_init(self, Blake2bParam, BLAKE2B_IV, out_len, key, salt, personal);

<*
 Core compression inline function for Blake2b.
*>
fn void Blake2b.compress(&self, char[BLAKE2B_BLOCKBYTES] block) @local @inline
	=> @common_compress(self, 12, BLAKE2B_IV, BLAKE2B_SIGMA, block);

<*
 Add more data to the hash context or stream.

 @param[in] data : "The data to ingest into the hash context."
*>
fn void Blake2b.update(&self, char[] data)
	=> @common_update(self, BLAKE2B_BLOCKBYTES, data);

<*
 Finalize the hash context and return the hash result at the given size.

 @param $output_length : "The length of the output array which is returned by value, instead of as a slice."

 @require $output_length == self.outlen : "The specified compile-time output size MUST be equal to the initialized output size."
*>
macro char[*] Blake2b.final(&self, $output_length)
	=> @common_final(self, $output_length);


// ======================================================================================
// BEGIN Blake2s contents. Do not separate this from Blake2b: there's not really a point.
//
const BLAKE2S_BLOCKBYTES	@local = BLAKE2B_BLOCKBYTES 	/ 2;
const BLAKE2S_OUTBYTES		@local = BLAKE2B_OUTBYTES 		/ 2;
const BLAKE2S_KEYBYTES		@local = BLAKE2B_KEYBYTES 		/ 2;
const BLAKE2S_SALTBYTES		@local = BLAKE2B_SALTBYTES 		/ 2;
const BLAKE2S_PERSONALBYTES	@local = BLAKE2B_PERSONALBYTES 	/ 2;
const uint[8] BLAKE2S_IV @local = { 0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19 };
const char[16][10] BLAKE2S_SIGMA @local = {
    x'000102030405060708090a0b0c0d0e0f',
    x'0e0a0408090f0d06010c00020b070503',
    x'0b080c0005020f0d0a0e030607010904',
    x'070903010d0c0b0e0206050a04000f08',
    x'0900050702040a0f0e010b0c0608030d',
    x'020c060a000b0803040d07050f0e0109',
    x'0c05010f0e0d040a000706030902080b',
    x'0d0b070e0c01030905000f040806020a',
    x'060f0e090b0300080c020d0701040a05',
    x'0a020804070601050f0b090e030c0d00',
};

struct Blake2s
{
	uint[8]						h;
	uint[2]						t;
	uint[2]						f;
	char[BLAKE2S_BLOCKBYTES]	buf;
	usz							buflen;
	usz							outlen;
	char						last_node;
}

struct Blake2sParam @packed @local
{
	char						digest_length;	/*  1 */
	char						key_length;		/*  2 */
	char						fanout;			/*  3 */
	char						depth;			/*  4 */
	uint						leaf_length;	/*  8 */
	uint						node_offset;	/* 12 */
	ushort						xof_length;		/* 14 */
	char						node_depth;		/* 15 */
	char						inner_length;	/* 16 */
	char[BLAKE2S_SALTBYTES]		salt;			/* 24 */
	char[BLAKE2S_PERSONALBYTES]	personal;		/* 32 */
}

<*
 @require $defined(data[0]) &&& $typeof(data[0]) == char : "Input data must be a char slice, char array, or string value."
*>
macro blake2s_hash($out_len, data, char[] key = {}, char[] salt = {})
{
	Blake2s b @noinit;
    b.init($out_len, key, salt);
	b.update(data[..]);
	return b.final($out_len);
}
alias s = blake2s_hash;

// See RFC 7693, Section 4 for common parameter sets.
macro blake2s_128(data, char[] key = {}, char[] salt = {}) => blake2s_hash(SIZE_128, data, key, salt);
macro blake2s_160(data, char[] key = {}, char[] salt = {}) => blake2s_hash(SIZE_160, data, key, salt);
macro blake2s_224(data, char[] key = {}, char[] salt = {}) => blake2s_hash(SIZE_224, data, key, salt);
macro blake2s_256(data, char[] key = {}, char[] salt = {}) => blake2s_hash(SIZE_256, data, key, salt);

alias s_128 = blake2s_128;
alias s_160 = blake2s_160;
alias s_224 = blake2s_224;
alias s_256 = blake2s_256;

<*
 Blake2s initialization method. Presents various options 

 @param out_len : "The desired output length from the hash function."
 @param[in] key : "An optional key value to use (keys the entire hash value to give HMAC-like functionality)."
 @param[in] salt : "An optional salt value to use when generating the hash."
 @param[in] personal : "An optional personalization value to use when generating the hash."

 @require out_len > 0 && out_len <= BLAKE2S_OUTBYTES : "Blake2 output length must be within the proper range."
 @require !key.ptr || (key.len > 0 && key.len <= BLAKE2S_KEYBYTES) : "A specified key's length must be within the proper range."
 @require !salt.ptr || (salt.len > 0 && salt.len <= BLAKE2S_SALTBYTES) : "A specified salt's length must be within the proper range."
 @require !personal.ptr || (personal.len > 0 && personal.len <= BLAKE2B_PERSONALBYTES) : "A specified personalization's length must be within the proper range."
*>
fn void Blake2s.init(&self, usz out_len, char[] key = {}, char[] salt = {}, char[] personal = {})
	=> @common_init(self, Blake2sParam, BLAKE2S_IV, out_len, key, salt, personal);

<*
 Core compression inline function for Blake2s.
*>
fn void Blake2s.compress(&self, char[BLAKE2S_BLOCKBYTES] block) @local @inline
	=> @common_compress(self, 10, BLAKE2S_IV, BLAKE2S_SIGMA, block);

<*
 Add more data to the hash context or stream.

 @param[in] data : "The data to ingest into the hash context."
*>
fn void Blake2s.update(&self, char[] data)
	=> @common_update(self, BLAKE2S_BLOCKBYTES, data);

<*
 Finalize the hash context and return the hash result at the given size.

 @param $output_length : "The length of the output array which is returned by value, instead of as a slice."

 @require $output_length == self.outlen : "The specified compile-time output size MUST be equal to the initialized output size."
*>
macro char[*] Blake2s.final(&self, $output_length)
	=> @common_final(self, $output_length);
