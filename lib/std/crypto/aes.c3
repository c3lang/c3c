<*
This is an implementation of the AES algorithm with the ECB, CTR and CBC
modes. The key size can be chosen among AES128, AES192, AES256.

Ported from github.com/kokke/tiny-aes-c by Koni Marti.

The implementation is verified against the test vectors from the National
Institute of Standards and Technology Special Publication 800-38A 2001 ED.

Data length must be evenly divisible by 16 bytes (len % 16 == 0) unless CTR is
used. You should pad the end of the string with zeros or use PKCS7 if this is not the case.
For AES192/256 the key size is proportionally larger.

The following example demonstrates the AES encryption of a plaintext string
with an AES 128-bit key:

```
module app;
import std::crypto::aes;
fn void main()
{
	char[] key 	= x"2b7e151628aed2a6abf7158809cf4f3c";
	char[] text	= x"6bc1bee22e409f96e93d7e117393172a";
	char[16] iv = x"f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";
	Aes aes;
	aes.init(AES128, key, iv);
	defer aes.destroy();
	char[] cipher = aes.encrypt(mem, text);
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

enum BlockMode
{
	ECB,
	CBC,
	CTR,
}

enum AesType : (AesKey key)
{
	AES128 = { 128, 16, 176, 4, 10 },
	AES192 = { 192, 24, 208, 6, 12 },
	AES256 = { 256, 32, 240, 8, 14 }
}

struct AesKey
{
	usz key_size;
	int key_len;
	int key_exp_size; // expected size of round_key
	usz nk;		      // number of 32 bit words in a key.
	usz nr;           // number of rounds in AES cipher.
}

struct Aes
{
	AesKey type;
	BlockMode mode;
	char[256] round_key;
	char[BLOCKLEN] iv;
	char[COLNUM][COLNUM] state;
}

<*
@require key.len == type.key.key_len : "Key does not match expected size."
*>
fn Aes* Aes.init(&self, AesType type, BlockMode mode, char[] key)
{
	self.type = type.key;
	self.mode = mode;
	self.type.key_expansion(&self.round_key, key);
	return self;
}

fn void Aes.destroy(&self)
{
	*self = {};
}

<*
@require key.len == type.key.key_len : "Key does not match expected length."
*>
fn Aes* Aes.init_with_iv(&self, AesType type, BlockMode mode, char[] key, char[BLOCKLEN] iv)
{
	self.init(type, mode, key);
	self.iv = iv;
	return self;
}


<*
@require iv.len == BLOCKLEN : "iv length should be equal to BLOCKLEN."
*>
fn Aes* Aes.set_iv(&self, char[16] iv)
{
	self.iv = iv;
	return self;
}
macro bool is_valid_encryption_len(BlockMode mode, usz len)
{
	switch (mode)
	{
		case CTR:
			return true;
		case ECB:
		case CBC:
			return len % BLOCKLEN == 0;
	}
}

<*
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
 @require is_valid_encryption_len(self.mode, in.len) : "The input must be a multiple of 16 unless CTR is used"
 @require out.len >= in.len : "Out buffer must be sufficiently large to hold the data"
*>
fn void Aes.encrypt_buffer(&self, char[] in, char[] out)
{
	switch (self.mode)
	{
		case CTR: ctr_xcrypt_buffer(self, in, out);
		case ECB: ecb_encrypt_buffer(self, in, out);
		case CBC: cbc_encrypt_buffer(self, in, out);
	}
}

<*
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
 @require is_valid_encryption_len(self.mode, in.len) : "The encrypted data must be a multiple of 16 unless CTR is used"
 @require out.len >= in.len : "Out buffer must be sufficiently large to hold the data"
*>
fn void Aes.decrypt_buffer(&self, char[] in, char[] out)
{
	switch (self.mode)
	{
		case ECB: ecb_decrypt_buffer(self, in, out);
		case CBC: cbc_decrypt_buffer(self, in, out);
		case CTR: ctr_xcrypt_buffer(self, in, out);
	}
}

<*
 Encrypt the data, allocating memory for the encrypted data.

 @param [in] in : "Plaintext input."
 @param [&inout] allocator : "The allocator to use for the output"
 @require is_valid_encryption_len(self.mode, in.len) : "The in-data needs to be a multiple of 16 unless CTR is used"
*>
fn char[] Aes.encrypt(&self, Allocator allocator, char[] in)
{
	char[] out = allocator::alloc_array(allocator, char, in.len);
	self.encrypt_buffer(in, out) @inline;
	return out;
}

<*
 Encrypt the data, allocating temp memory for the encrypted data.

 @param [in] in : "Plaintext input."
 @require is_valid_encryption_len(self.mode, in.len) : "The in-data needs to be a multiple of 16 unless CTR is used"
*>
fn char[] Aes.tencrypt(&self, char[] in)
{
	return self.encrypt(tmem, in);
}

<*
 Decrypt the data, allocating memory for the decrypted data.

 @param [in] in : "Encrypted input."
 @param [&inout] allocator : "The allocator to use for the output"
 @require is_valid_encryption_len(self.mode, in.len) : "The in-data needs to be a multiple of 16 unless CTR is used"
*>
fn char[] Aes.decrypt(&self, Allocator allocator, char[] in)
{
	char[] out = allocator::alloc_array(allocator, char, in.len);
	self.decrypt_buffer(in, out) @inline;
	return out;
}

<*
 Decrypt the data, allocating temp memory for the decrypted data.

 @param [in] in : "Encrypted input."
 @require is_valid_encryption_len(self.mode, in.len) : "The in-data needs to be a multiple of 16 unless CTR is used"

*>
fn char[] Aes.tdecrypt(&self, char[] in)
{
	return self.decrypt(tmem, in);
}

module std::crypto::aes @private;

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void ecb_encrypt_block(Aes *ctx, char[BLOCKLEN]* in, char[BLOCKLEN]* out)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			ctx.state[i][j] = (*in)[i * 4 + j];
		}
	}
	ctx.cipher(&ctx.round_key);
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			(*out)[i * 4 + j] = ctx.state[i][j];
		}
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void ecb_decrypt_block(Aes *ctx, char[BLOCKLEN]* in, char[BLOCKLEN]* out)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			ctx.state[i][j] = (*in)[i * 4 + j];
		}
	}
	ctx.inv_cipher(&ctx.round_key);
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			(*out)[i * 4 + j] = ctx.state[i][j];
		}
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
 @require out.len >= in.len : "out must be at least as large as buf"
*>
fn void ecb_decrypt_buffer(Aes *ctx, char[] in, char[] out)
{
	usz len = in.len;
	for (usz i = 0; i < len; i += 4)
	{
		ecb_decrypt_block(ctx, in[:BLOCKLEN], out[:BLOCKLEN]) @inline;
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void ecb_encrypt_buffer(Aes *ctx, char[] in, char[] out)
{
	usz len = in.len;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		ecb_encrypt_block(ctx, in[i:BLOCKLEN], out[i:BLOCKLEN]) @inline;
	}
}

fn void xor_with_iv(char[] buf, char[BLOCKLEN]* iv) @local
{
	foreach (i, b : *iv)
	{
		buf[i] ^= b;
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void cbc_encrypt_buffer(Aes *ctx, char[] in, char[] out)
{
	char[] iv = ctx.iv[..];
	usz len = in.len;
	char[BLOCKLEN] tmp;
	char[BLOCKLEN] tmp2;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		tmp[:BLOCKLEN] = in[i:BLOCKLEN];
		xor_with_iv(&tmp, iv);
		ecb_encrypt_block(ctx, &tmp, &tmp2);
		out[i:BLOCKLEN] = tmp2[..];
		iv = tmp2[..];
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void cbc_decrypt_buffer(Aes *ctx, char[] in, char[] out)
{
	char[BLOCKLEN] tmp;
	usz len = in.len;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		ecb_decrypt_block(ctx, in[i:BLOCKLEN], &tmp);
		xor_with_iv(&tmp, ctx.iv[..]);
		ctx.iv[:BLOCKLEN] = in[i:BLOCKLEN];
		out[i:BLOCKLEN] = tmp[..];
	}
}

<*
 @param [&inout] ctx : "AES context."
 @param [in] in : "Plaintext/cipher input."
 @param [out] out : "Cipher/plaintext output."
*>
fn void ctr_xcrypt_buffer(Aes *ctx, char[] in, char[] out)
{
	 char[BLOCKLEN] buffer;
	 usz len = in.len;
	 for (int bi = BLOCKLEN, usz i = 0; i < len; i++)
	 {
		 if (bi == BLOCKLEN)
		 {
			 buffer = ctx.iv;
			 ecb_encrypt_block(ctx, &buffer, &buffer);

			 for LOOP: (bi = (BLOCKLEN - 1); bi >= 0; bi--)
			 {
				 if (ctx.iv[bi] == 255)
				 {
					 ctx.iv[bi] = 0;
					 continue;
				 }
				 ctx.iv[bi]++;
				 break LOOP;
			 }
			 bi = 0;
		 }
		 out[i] = in[i] ^ buffer[bi];
		 bi++;
	 }
}


/*
 *
 * Aes implementation
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

fn void Aes.add_round_key(&self, usz round, char[] round_key) @private
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

fn void Aes.sub_bytes(&self) @private
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			self.state[j][i] = get_sbox_value(self.state[j][i]);
		}
	}
}

fn void Aes.shift_rows(&self) @private
{
	char temp;

	temp             = self.state[0][1];
	self.state[0][1] = self.state[1][1];
	self.state[1][1] = self.state[2][1];
	self.state[2][1] = self.state[3][1];
	self.state[3][1] = temp;

	temp	         = self.state[0][2];
	self.state[0][2] = self.state[2][2];
	self.state[2][2] = temp;

	temp	         = self.state[1][2];
	self.state[1][2] = self.state[3][2];
	self.state[3][2] = temp;

	temp	         = self.state[0][3];
	self.state[0][3] = self.state[3][3];
	self.state[3][3] = self.state[2][3];
	self.state[2][3] = self.state[1][3];
	self.state[1][3] = temp;
}

fn char xtime(char x) @local
{
	return ((x << 1) ^ (((x >> 7) & 1) * 0x1b));
}

fn void Aes.mix_columns(&self) @private
{
	for (usz i = 0; i < 4; i++)
	{
		char t = self.state[i][0];
		char tmp = self.state[i][0] ^ self.state[i][1] ^ self.state[i][2] ^ self.state[i][3];

		char tm = self.state[i][0] ^ self.state[i][1];
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

fn void Aes.inv_mix_columns(&self) @private
{
	for (int i = 0; i < 4; i++)
	{
		char a = self.state[i][0];
		char b = self.state[i][1];
		char c = self.state[i][2];
		char d = self.state[i][3];

		self.state[i][0] = multiply(a, 0x0e) ^ multiply(b, 0x0b) ^ multiply(c, 0x0d) ^ multiply(d, 0x09);
		self.state[i][1] = multiply(a, 0x09) ^ multiply(b, 0x0e) ^ multiply(c, 0x0b) ^ multiply(d, 0x0d);
		self.state[i][2] = multiply(a, 0x0d) ^ multiply(b, 0x09) ^ multiply(c, 0x0e) ^ multiply(d, 0x0b);
		self.state[i][3] = multiply(a, 0x0b) ^ multiply(b, 0x0d) ^ multiply(c, 0x09) ^ multiply(d, 0x0e);
	}
}

fn void Aes.inv_sub_bytes(&self) @private
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			self.state[j][i] = get_sbox_invert(self.state[j][i]);
		}
	}
}

fn void Aes.inv_shift_rows(&self) @private
{
	char temp;

	temp             = self.state[3][1];
	self.state[3][1] = self.state[2][1];
	self.state[2][1] = self.state[1][1];
	self.state[1][1] = self.state[0][1];
	self.state[0][1] = temp;

	temp	         = self.state[0][2];
	self.state[0][2] = self.state[2][2];
	self.state[2][2] = temp;

	temp	         = self.state[1][2];
	self.state[1][2] = self.state[3][2];
	self.state[3][2] = temp;

	temp	         = self.state[0][3];
	self.state[0][3] = self.state[1][3];
	self.state[1][3] = self.state[2][3];
	self.state[2][3] = self.state[3][3];
	self.state[3][3] = temp;
}

fn void Aes.cipher(&self, char[] round_key)
{
	usz round = 0;
	self.add_round_key(0, round_key);

	for LOOP: (round = 1;; round++)
	{
		self.sub_bytes();
		self.shift_rows();
		if (round == self.type.nr) break LOOP;
		self.mix_columns();
		self.add_round_key(round, round_key);
	}
	self.add_round_key(self.type.nr, round_key);
}

fn void Aes.inv_cipher(&self, char[] round_key)
{
	self.add_round_key(self.type.nr, round_key);
	for (usz round = self.type.nr - 1; ; round--)
	{
		self.inv_shift_rows();
		self.inv_sub_bytes();
		self.add_round_key(round, round_key);
		if (!round) return;
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
	for (usz i = 0; i < self.nk; i++)
	{
		round_key[(i * 4) + 0] = key[(i * 4) + 0];
		round_key[(i * 4) + 1] = key[(i * 4) + 1];
		round_key[(i * 4) + 2] = key[(i * 4) + 2];
		round_key[(i * 4) + 3] = key[(i * 4) + 3];
	}


	for (usz i = self.nk; i < COLNUM * (self.nr + 1); i++)
	{
		usz k = (i - 1) * 4;

		char[4] tempa @noinit;

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
		usz j = i * 4;
		k = (i - self.nk) * 4;
		round_key[j + 0] = round_key[k + 0] ^ tempa[0];
		round_key[j + 1] = round_key[k + 1] ^ tempa[1];
		round_key[j + 2] = round_key[k + 2] ^ tempa[2];
		round_key[j + 3] = round_key[k + 3] ^ tempa[3];
	}
}

