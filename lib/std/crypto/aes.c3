<*
This is an implementation of the AES algorithm with the ECB, CTR and CBC
modes. The key size can be chosen among AES128, AES192, AES256.

Ported from github.com/kokke/tiny-aes-c by Koni Marti.

The implementation is verified against the test vectors from the National
Institute of Standards and Technology Special Publication 800-38A 2001 ED.

String length must be evenly divisible by 16 bytes (str_len % 16 == 0).
You should pad the end of the string with zeros if this is not the case.
For AES192/256 the key size is proportionally larger.

Each AES mode defines a namespace (`std::crypto::aes::ecb`,
`std::crypto::aes::cbc`, `std::crypto::aes::ctr`) with the following function
signatures:

```
fn void encrypt_buffer(AesCtx *ctx, char[] text, char[] cipher)
fn void decrypt_buffer(AesCtx *ctx, char[] cipher, char[] text)

fn char[] encrypt(AesCtx *ctx, char[] text, Allocator allocator = mem)
fn char[] decrypt(AesCtx *ctx, char[] cipher, Allocator allocator = mem)

fn char[] tencrypt(AesCtx *ctx, char[] text)
fn char[] tdecrypt(AesCtx *ctx, char[] cipher)
```

The following example demonstrates the AES encryption of a plaintext string
with an AES 128-bit key:

```
module app;
import std::crypto::aes;
fn void main()
{
	char[] key 	= x"2b7e151628aed2a6abf7158809cf4f3c";
	char[] text	= x"6bc1bee22e409f96e93d7e117393172a";
	char[] cipher = ecb::encrypt((AesCtx){}.init(aes::AES128, key), text);
	defer free(cipher);
	assert(cipher == x"3ad77bb40d7a3660a89ecaf32466ef97");
}
```

*>
module std::crypto::aes;

<* Block length in bytes. AES is 128-bit blocks only. *>
const BLOCKLEN = 16;

<* Number of columns of a AES state. *>
const COLNUM = 4;

struct AesKey
{
	usz key_size;
	int key_len;
	int key_exp_size; // expected size of round_key
	usz nk;		  // number of 32 bit words in a key.
	usz nr;           // number of rounds in AES cipher.
}

const AesKey AES128 = { 128, 16, 176, 4, 10 };
const AesKey AES192 = { 192, 24, 208, 6, 12 };
const AesKey AES256 = { 256, 32, 240, 8, 14 };

/*
 *
 * AesCtx
 *
 */

faultdef AES_IV_TOO_SHORT;

struct AesCtx
{
	AesKey type;
	char[256] round_key;
	char[BLOCKLEN] iv;
	char[COLNUM][COLNUM] state;
}

<*
@require key.len == type.key_len : "Key does not match expected size."
*>
fn AesCtx* AesCtx.init(&self, AesKey type, char[] key)
{
	self.type = type;
	self.type.key_expansion(&self.round_key, key);
	return self;
}

<*
@require key.len == type.key_len : "Key does not match expected length."
@require iv.len == BLOCKLEN : "iv length should be equal to BLOCKLEN."
*>
fn AesCtx*? AesCtx.init_with_iv(&self, AesKey type, char[] key, char[] iv)
{
	self.init(type, key);
	if (iv.len < BLOCKLEN)
	{
		return AES_IV_TOO_SHORT?;
	}
	self.iv[:BLOCKLEN] = iv[:BLOCKLEN];
	return self;
}

<*
@require iv.len == BLOCKLEN : "iv length should be equal to BLOCKLEN."
*>
fn AesCtx* AesCtx.set_iv(&self, char[] iv)
{
	self.iv[:BLOCKLEN] = iv[:BLOCKLEN];
	return self;
}

/*
 *
 * ECB
 *
 */
module std::crypto::aes::ecb;

fn void encrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => ecb_encrypt(ctx, buf, out);
fn void decrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => ecb_decrypt(ctx, buf, out);

fn char[] encrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, 16);
	encrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] decrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, 16);
	decrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] tencrypt(AesCtx *ctx, char[] buf) @inline => encrypt(ctx, buf, tmem);
fn char[] tdecrypt(AesCtx *ctx, char[] buf) @inline => decrypt(ctx, buf, tmem);

<*
@param [&inout] ctx : "AES context."
@param [in] buf : "Plaintext input."
@param [out] out : "Cipher output."
@require buf.len == 16, out.len == 16
*>
fn void ecb_encrypt(AesCtx *ctx, char[] buf, char[] out)
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			ctx.state[i][j] = buf[i*4+j];
		}
	}
	ctx.cipher(&ctx.round_key);
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			out[i*4+j] = ctx.state[i][j];
		}
	}
}

<*
@param [&inout] ctx : "AES context."
@param [in] buf : "Plaintext input."
@param [out] out : "Cipher output."
@require buf.len == 16, out.len == 16
*>
fn void ecb_decrypt(AesCtx *ctx, char[] buf, char[] out)
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			ctx.state[i][j] = buf[i*4+j];
		}
	}
	ctx.inv_cipher(&ctx.round_key);
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			out[i*4+j] = ctx.state[i][j];
		}
	}
}

/*
 *
 * CBC
 *
 */
module std::crypto::aes::cbc;

import std::crypto::aes::ecb;

fn void encrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => cbc_encrypt_buffer(ctx, buf, out);
fn void decrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => cbc_decrypt_buffer(ctx, buf, out);

fn char[] encrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, buf.len);
	encrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] decrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, buf.len);
	decrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] tencrypt(AesCtx *ctx, char[] buf) @inline => encrypt(ctx, buf, tmem);
fn char[] tdecrypt(AesCtx *ctx, char[] buf) @inline => decrypt(ctx, buf, tmem);

fn void xor_with_iv(char[] buf, char[] iv) @local
{
	usz i;
	for (i = 0; i < aes::BLOCKLEN; i++)
	{
		buf[i] ^= iv[i];
	}
}

<*
@param [&inout] ctx : "AES context."
@param [in] buf : "Plaintext input."
@param [out] out : "Cipher output."
@require buf.len % aes::BLOCKLEN == 0 : "buf must be a multiple of 16."
@require out.len % aes::BLOCKLEN == 0 : "out must be a multiple of 16."
*>
fn void cbc_encrypt_buffer(AesCtx *ctx, char[] buf, char[] out)
{
	usz i;
	char[] iv = ctx.iv[..];
	char[aes::BLOCKLEN] tmp;
	char[aes::BLOCKLEN] tmp2;
	for (i = 0; i < buf.len; i += aes::BLOCKLEN)
	{
		tmp[:aes::BLOCKLEN] = buf[i:aes::BLOCKLEN];
		xor_with_iv(&tmp, iv);
		ecb::encrypt_buffer(ctx, &tmp, &tmp2);
		out[i:aes::BLOCKLEN] = tmp2[..];
		iv[:aes::BLOCKLEN] = tmp2[..];
	}
	// store iv in ctx for next call
	ctx.iv[:aes::BLOCKLEN] = iv[:aes::BLOCKLEN];
}

<*
@param [&inout] ctx : "AES context."
@param [in] buf : "Cipher input."
@param [out] out : "Plaintext output."
@require buf.len % aes::BLOCKLEN == 0 : "buf must be a multiple of 16."
@require out.len % aes::BLOCKLEN == 0 : "out must be a multiple of 16."
*>
fn void cbc_decrypt_buffer(AesCtx *ctx, char[] buf, char[] out)
{
	usz i;
	char[aes::BLOCKLEN] tmp;
	for (i = 0; i < buf.len; i += aes::BLOCKLEN)
	{
		ecb::decrypt_buffer(ctx, buf[i:aes::BLOCKLEN], &tmp);
		xor_with_iv(&tmp, ctx.iv[..]);
		ctx.iv[:aes::BLOCKLEN] = buf[i:aes::BLOCKLEN];
		out[i:aes::BLOCKLEN] = tmp[..];
	}
}

/*
 *
 * CTR
 *
 */
module std::crypto::aes::ctr;

import std::crypto::aes::ecb;

fn void encrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => ctr_xcrypt_buffer(ctx, buf, out);
fn void decrypt_buffer(AesCtx *ctx, char[] buf, char[] out) @inline => ctr_xcrypt_buffer(ctx, buf, out);

fn char[] encrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, buf.len);
	encrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] decrypt(AesCtx *ctx, char[] buf, Allocator allocator = mem) {
	char[] out = allocator::alloc_array(allocator, char, buf.len);
	decrypt_buffer(ctx, buf, out);
	return out;
}

fn char[] tencrypt(AesCtx *ctx, char[] buf) @inline => encrypt(ctx, buf, tmem);
fn char[] tdecrypt(AesCtx *ctx, char[] buf) @inline => decrypt(ctx, buf, tmem);

<*
@param [&inout] ctx : "AES context."
@param [in] buf : "Plaintext input."
@param [out] out : "Cipher output."
@require buf.len % aes::BLOCKLEN == 0 : "buf must be a multiple of 16."
@require out.len % aes::BLOCKLEN == 0 : "out must be a multiple of 16."
*>
fn void ctr_xcrypt_buffer(AesCtx *ctx, char[] buf, char[] out)
{
	 char[aes::BLOCKLEN] buffer;

	 usz i;
	 int bi = aes::BLOCKLEN;
	 for (i = 0; i < buf.len; i++)
	 {
		 if (bi == aes::BLOCKLEN)
		 {
			 buffer[..] = ctx.iv[..];
			 ecb::encrypt_buffer(ctx, &buffer, &buffer);

			 for LOOP: (bi = (aes::BLOCKLEN - 1); bi >= 0; bi--)
			 {
				 if (ctx.iv[bi] == 255)
				 {
					 ctx.iv[bi] = 0;
					 continue;
				 }
				 ctx.iv[bi] += 1;
				 break LOOP;
			 }
			 bi = 0;
		 }
		 out[i] = (buf[i] ^ buffer[bi]);
		 bi++;
	 }
}



/*
 *
 * AesCtx implementation
 *
 */
module std::crypto::aes;

macro char get_sbox_value(num) => SBOX[num];
macro char get_sbox_invert(num) => RSBOX[num];

const char[256] SBOX =
	x`637c777bf26b6fc53001672bfed7ab76
	  ca82c97dfa5947f0add4a2af9ca472c0
	  b7fd9326363ff7cc34a5e5f171d83115
	  04c723c31896059a071280e2eb27b275
	  09832c1a1b6e5aa0523bd6b329e32f84
	  53d100ed20fcb15b6acbbe394a4c58cf
	  d0efaafb434d338545f9027f503c9fa8
	  51a3408f929d38f5bcb6da2110fff3d2
	  cd0c13ec5f974417c4a77e3d645d1973
	  60814fdc222a908846eeb814de5e0bdb
	  e0323a0a4906245cc2d3ac629195e479
	  e7c8376d8dd54ea96c56f4ea657aae08
	  ba78252e1ca6b4c6e8dd741f4bbd8b8a
	  703eb5664803f60e613557b986c11d9e
	  e1f8981169d98e949b1e87e9ce5528df
	  8ca1890dbfe6426841992d0fb054bb16`;

const char[256] RSBOX =
	x`52096ad53036a538bf40a39e81f3d7fb
	  7ce339829b2fff87348e4344c4dee9cb
	  547b9432a6c2233dee4c950b42fac34e
	  082ea16628d924b2765ba2496d8bd125
	  72f8f66486689816d4a45ccc5d65b692
	  6c704850fdedb9da5e154657a78d9d84
	  90d8ab008cbcd30af7e45805b8b34506
	  d02c1e8fca3f0f02c1afbd0301138a6b
	  3a9111414f67dcea97f2cfcef0b4e673
	  96ac7422e7ad3585e2f937e81c75df6e
	  47f11a711d29c5896fb7620eaa18be1b
	  fc563e4bc6d279209adbc0fe78cd5af4
	  1fdda8338807c731b11210592780ec5f
	  60517fa919b54a0d2de57a9f93c99cef
	  a0e03b4dae2af5b0c8ebbb3c83539961
	  172b047eba77d626e169146355210c7d`;

const char[11] RCON = x`8d01020408102040801b36`;

fn void AesCtx.add_round_key(&self, usz round, char[] round_key) @private
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			self.state[i][j] ^= round_key[(round * aes::COLNUM * 4) + (i * aes::COLNUM) + j];
		}
	}
}

fn void AesCtx.sub_bytes(&self) @private
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			self.state[j][i] = get_sbox_value(self.state[j][i]);
		}
	}
}

fn void AesCtx.shift_rows(&self) @private
{
	char temp;

	temp        = self.state[0][1];
	self.state[0][1] = self.state[1][1];
	self.state[1][1] = self.state[2][1];
	self.state[2][1] = self.state[3][1];
	self.state[3][1] = temp;

	temp	    = self.state[0][2];
	self.state[0][2] = self.state[2][2];
	self.state[2][2] = temp;

	temp	    = self.state[1][2];
	self.state[1][2] = self.state[3][2];
	self.state[3][2] = temp;

	temp	    = self.state[0][3];
	self.state[0][3] = self.state[3][3];
	self.state[3][3] = self.state[2][3];
	self.state[2][3] = self.state[1][3];
	self.state[1][3] = temp;
}

fn char xtime(char x) @local
{
	return ((x << 1) ^ (((x >> 7) & 1) * 0x1b));
}

fn void AesCtx.mix_columns(&self) @private
{
	usz i;
	char tmp, tm, t;
	for (i = 0; i < 4; i++)
	{
		t = self.state[i][0];
		tmp = self.state[i][0] ^ self.state[i][1] ^ self.state[i][2] ^ self.state[i][3];

		tm = self.state[i][0] ^ self.state[i][1];
		tm = xtime(tm);
		self.state[i][0] ^= tm ^ tmp;

		tm = self.state[i][1] ^ self.state[i][2];
		tm = xtime(tm);
		self.state[i][1] ^= tm ^ tmp;

		tm = self.state[i][2] ^ self.state[i][3];
		tm = xtime(tm);
		self.state[i][2] ^= tm ^ tmp;

		tm = self.state[i][3] ^ t;
		tm = xtime(tm);
		self.state[i][3] ^= tm ^ tmp;
	}
}

fn char multiply(char x, char y) @local
{
	return (((y & 1) * x) ^
	(((y>>1) & 1) * xtime(x)) ^
	(((y>>2) & 1) * xtime(xtime(x))) ^
	(((y>>3) & 1) * xtime(xtime(xtime(x)))) ^
	(((y>>4) & 1) * xtime(xtime(xtime(xtime(x))))));
}

fn void AesCtx.inv_mix_columns(&self) @private
{
	usz i;
	char a, b, c, d;
	for (i = 0; i < 4; i++)
	{
		a = self.state[i][0];
		b = self.state[i][1];
		c = self.state[i][2];
		d = self.state[i][3];

		self.state[i][0] = multiply(a, 0x0e) ^ multiply(b, 0x0b) ^ multiply(c, 0x0d) ^ multiply(d, 0x09);
		self.state[i][1] = multiply(a, 0x09) ^ multiply(b, 0x0e) ^ multiply(c, 0x0b) ^ multiply(d, 0x0d);
		self.state[i][2] = multiply(a, 0x0d) ^ multiply(b, 0x09) ^ multiply(c, 0x0e) ^ multiply(d, 0x0b);
		self.state[i][3] = multiply(a, 0x0b) ^ multiply(b, 0x0d) ^ multiply(c, 0x09) ^ multiply(d, 0x0e);
	}
}

fn void AesCtx.inv_sub_bytes(&self) @private
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			self.state[j][i] = get_sbox_invert(self.state[j][i]);
		}
	}
}

fn void AesCtx.inv_shift_rows(&self) @private
{
	char temp;

	temp        = self.state[3][1];
	self.state[3][1] = self.state[2][1];
	self.state[2][1] = self.state[1][1];
	self.state[1][1] = self.state[0][1];
	self.state[0][1] = temp;

	temp	    = self.state[0][2];
	self.state[0][2] = self.state[2][2];
	self.state[2][2] = temp;

	temp	    = self.state[1][2];
	self.state[1][2] = self.state[3][2];
	self.state[3][2] = temp;

	temp	    = self.state[0][3];
	self.state[0][3] = self.state[1][3];
	self.state[1][3] = self.state[2][3];
	self.state[2][3] = self.state[3][3];
	self.state[3][3] = temp;
}

fn void AesCtx.cipher(&self, char[] round_key)
{
	usz round = 0;

	self.add_round_key(0, round_key);

	for LOOP: (round = 1; ; round++)
	{
		self.sub_bytes();
		self.shift_rows();
		if (round == self.type.nr) break LOOP;
		self.mix_columns();
		self.add_round_key(round, round_key);
	}
	self.add_round_key(self.type.nr, round_key);
}

fn void AesCtx.inv_cipher(&self, char[] round_key)
{
	usz round = 0;

	self.add_round_key(self.type.nr, round_key);

	for LOOP: (round = self.type.nr - 1; ; round--)
	{
		self.inv_shift_rows();
		self.inv_sub_bytes();
		self.add_round_key(round, round_key);
		if (round == 0) break LOOP;
		self.inv_mix_columns();
	}
}

/*
 *
 * AesKey
 *
 */

<*
@require key.len == self.key_len : "Key does not match expected length."
*>
fn void AesKey.key_expansion(&self, char[] round_key, char[] key) @private
{
	usz i, j, k;
	char[4] tempa;

	for (i = 0; i < self.nk; i++)
	{
		round_key[(i * 4) + 0] = key[(i * 4) + 0];
		round_key[(i * 4) + 1] = key[(i * 4) + 1];
		round_key[(i * 4) + 2] = key[(i * 4) + 2];
		round_key[(i * 4) + 3] = key[(i * 4) + 3];
	}

	for (i = self.nk; i < aes::COLNUM * (self.nr + 1); i++)
	{
		k = (i - 1) * 4;
		tempa[0] = round_key[k + 0];
		tempa[1] = round_key[k + 1];
		tempa[2] = round_key[k + 2];
		tempa[3] = round_key[k + 3];

		if (i % self.nk == 0)
		{
			// rotword
			char tmp = tempa[0];
			tempa[0] = tempa[1];
			tempa[1] = tempa[2];
			tempa[2] = tempa[3];
			tempa[3] = tmp;

			// subword
			tempa[0] = get_sbox_value(tempa[0]);
			tempa[1] = get_sbox_value(tempa[1]);
			tempa[2] = get_sbox_value(tempa[2]);
			tempa[3] = get_sbox_value(tempa[3]);

			tempa[0] = tempa[0] ^ RCON[i/self.nk];
		}

		if (self.key_size == 256)
		{
			if (i % self.nk == 4)
			{
				// subword
				tempa[0] = get_sbox_value(tempa[0]);
				tempa[1] = get_sbox_value(tempa[1]);
				tempa[2] = get_sbox_value(tempa[2]);
				tempa[3] = get_sbox_value(tempa[3]);
			}
		}
		j = i * 4;
		k = (i - self.nk) * 4;
		round_key[j + 0] = round_key[k + 0] ^ tempa[0];
		round_key[j + 1] = round_key[k + 1] ^ tempa[1];
		round_key[j + 2] = round_key[k + 2] ^ tempa[2];
		round_key[j + 3] = round_key[k + 3] ^ tempa[3];
	}
}

