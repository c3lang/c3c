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
import std::crypto::aes, std::io;
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
	assert(cipher == x"874d6191b620e3261bef6864990db6ce");
}
```

*>
module std::crypto::aes;

<* Block length in bytes. AES is 128-bit blocks only. *>
const BLOCKLEN = 16;

<* Number of columns of a AES state. *>
const COLNUM = 4;

<*
 Block modes:
 ECB - Electronic Code Book  (Not recommended, indata be 16 byte multiple)
 CBC - Cipher Block Chaining (Indata be 16 byte multiple)
 CTR - Counter Mode          (Recommended, data may be any size)
*>
enum BlockMode
{
	ECB,
	CBC,
	CTR,
}

<* AES type: 128, 192 or 256 bits *>
enum AesType : (AesKey key)
{
	AES128 = { 128, 16, 176, 4, 10 },
	AES192 = { 192, 24, 208, 6, 12 },
	AES256 = { 256, 32, 240, 8, 14 }
}

struct AesKey
{
	<* Size of key in bits *>
	usz key_size;
	<* Size of key in bytes *>
	int key_len;
	<* Size of the expanded round_key *>
	int key_exp_size; // expected size of round_key
	<* Number of 32 bit words in key *>
	usz nk;
	<* Number of rounds in the cipher *>
	usz nr;
}

struct Aes
{
	<* The type, AES128, AES192 or AES256 *>
	AesKey type;
	<* Block mode: ECB, CBC or CTR *>
	BlockMode mode;
	<* Initialization Vector *>
	char[BLOCKLEN] iv;
	<* Internal key state *>
	char[256] round_key;
	<* Internal state *>
	AesState state;
}
alias AesState = char[COLNUM][COLNUM];

<*
 Initializes the AES crypto. The initialization vector should be securely random for each encryption
 to mitigate things like replay attacks.

 @param type : "The type or AES: 128, 192 or 256 bits"
 @param [in] key : "The key to use, should be the same bit size as the type, so 16, 24 or 32 bytes"
 @param iv : "The initialization vector"
 @param mode : "The block mode: EBC, CBC, CTR. Defaults to CTR"

 @require key.len == type.key.key_len : "Key does not match expected length."
*>
fn Aes* Aes.init(&self, AesType type, char[] key, char[BLOCKLEN] iv, BlockMode mode = CTR)
{
	*self = { .type = type.key, .mode = mode, .iv = iv };
	key_expansion(type, key, &self.round_key);
	return self;
}

<*
 Completely erases data stored in the context.
*>
fn void Aes.destroy(&self)
{
	*self = {};
}

<*
 Check if the length is valid using the given block mode. It has to be a multiple of 16 bytes unless CTR is used.
*>
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
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void ecb_encrypt_block(Aes *aes, char[BLOCKLEN]* in, char[BLOCKLEN]* out)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			aes.state[i][j] = (*in)[i * 4 + j];
		}
	}
	aes_cipher(aes, &aes.round_key);
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			(*out)[i * 4 + j] = aes.state[i][j];
		}
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void ecb_decrypt_block(Aes *aes, char[BLOCKLEN]* in, char[BLOCKLEN]* out)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			aes.state[i][j] = (*in)[i * 4 + j];
		}
	}
	inv_cipher(aes, &aes.round_key);
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			(*out)[i * 4 + j] = aes.state[i][j];
		}
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
 @require out.len >= in.len : "out must be at least as large as buf"
*>
fn void ecb_decrypt_buffer(Aes *aes, char[] in, char[] out)
{
	usz len = in.len;
	for (usz i = 0; i < len; i += 4)
	{
		ecb_decrypt_block(aes, in[:BLOCKLEN], out[:BLOCKLEN]) @inline;
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void ecb_encrypt_buffer(Aes *aes, char[] in, char[] out)
{
	usz len = in.len;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		ecb_encrypt_block(aes, in[i:BLOCKLEN], out[i:BLOCKLEN]) @inline;
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
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void cbc_encrypt_buffer(Aes *aes, char[] in, char[] out)
{
	char[] iv = aes.iv[..];
	usz len = in.len;
	char[BLOCKLEN] tmp;
	char[BLOCKLEN] tmp2;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		tmp[:BLOCKLEN] = in[i:BLOCKLEN];
		xor_with_iv(&tmp, iv);
		ecb_encrypt_block(aes, &tmp, &tmp2);
		out[i:BLOCKLEN] = tmp2[..];
		iv = tmp2[..];
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void cbc_decrypt_buffer(Aes *aes, char[] in, char[] out)
{
	char[BLOCKLEN] tmp;
	usz len = in.len;
	for (usz i = 0; i < len; i += BLOCKLEN)
	{
		ecb_decrypt_block(aes, in[i:BLOCKLEN], &tmp);
		xor_with_iv(&tmp, aes.iv[..]);
		aes.iv[:BLOCKLEN] = in[i:BLOCKLEN];
		out[i:BLOCKLEN] = tmp[..];
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext/cipher input."
 @param [out] out : "Cipher/plaintext output."
*>
fn void ctr_xcrypt_buffer(Aes *aes, char[] in, char[] out)
{
	 char[BLOCKLEN] buffer @noinit;
	 usz len = in.len;
	 for (int bi = BLOCKLEN, usz i = 0; i < len; i++)
	 {
		 if (bi == BLOCKLEN)
		 {
			 buffer = aes.iv;
			 ecb_encrypt_block(aes, &buffer, &buffer);

			 for LOOP: (bi = (BLOCKLEN - 1); bi >= 0; bi--)
			 {
				 if (aes.iv[bi] == 255)
				 {
					 aes.iv[bi] = 0;
					 continue;
				 }
				 aes.iv[bi]++;
				 break LOOP;
			 }
			 bi = 0;
		 }
		 out[i] = in[i] ^ buffer[bi];
		 bi++;
	 }
}

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

fn void add_round_key(Aes* aes, usz round, char[] round_key)
{
	usz i, j;
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 4; j++)
		{
			aes.state[i][j] ^= round_key[(round * COLNUM * 4) + (i * COLNUM) + j];
		}
	}
}

fn void sub_bytes(Aes* aes)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			aes.state[j][i] = get_sbox_value(aes.state[j][i]);
		}
	}
}

fn void shift_rows(Aes* aes)
{
	char temp;

	temp             = aes.state[0][1];
	aes.state[0][1]  = aes.state[1][1];
	aes.state[1][1]  = aes.state[2][1];
	aes.state[2][1]  = aes.state[3][1];
	aes.state[3][1]  = temp;

	temp	         = aes.state[0][2];
	aes.state[0][2]  = aes.state[2][2];
	aes.state[2][2]  = temp;

	temp	         = aes.state[1][2];
	aes.state[1][2]  = aes.state[3][2];
	aes.state[3][2]  = temp;

	temp	         = aes.state[0][3];
	aes.state[0][3]  = aes.state[3][3];
	aes.state[3][3]  = aes.state[2][3];
	aes.state[2][3]  = aes.state[1][3];
	aes.state[1][3]  = temp;
}

fn char xtime(char x) @local
{
	return ((x << 1) ^ (((x >> 7) & 1) * 0x1b));
}

fn void mix_columns(Aes* aes)
{
	for (usz i = 0; i < 4; i++)
	{
		char t = aes.state[i][0];
		char tmp = aes.state[i][0] ^ aes.state[i][1] ^ aes.state[i][2] ^ aes.state[i][3];

		char tm = aes.state[i][0] ^ aes.state[i][1];
		tm = xtime(tm);
		aes.state[i][0] ^= tm ^ tmp;

		tm = aes.state[i][1] ^ aes.state[i][2];
		tm = xtime(tm);
		aes.state[i][1] ^= tm ^ tmp;

		tm = aes.state[i][2] ^ aes.state[i][3];
		tm = xtime(tm);
		aes.state[i][2] ^= tm ^ tmp;

		tm = aes.state[i][3] ^ t;
		tm = xtime(tm);
		aes.state[i][3] ^= tm ^ tmp;
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

fn void inv_mix_columns(Aes* aes)
{
	for (int i = 0; i < 4; i++)
	{
		char a = aes.state[i][0];
		char b = aes.state[i][1];
		char c = aes.state[i][2];
		char d = aes.state[i][3];

		aes.state[i][0] = multiply(a, 0x0e) ^ multiply(b, 0x0b) ^ multiply(c, 0x0d) ^ multiply(d, 0x09);
		aes.state[i][1] = multiply(a, 0x09) ^ multiply(b, 0x0e) ^ multiply(c, 0x0b) ^ multiply(d, 0x0d);
		aes.state[i][2] = multiply(a, 0x0d) ^ multiply(b, 0x09) ^ multiply(c, 0x0e) ^ multiply(d, 0x0b);
		aes.state[i][3] = multiply(a, 0x0b) ^ multiply(b, 0x0d) ^ multiply(c, 0x09) ^ multiply(d, 0x0e);
	}
}

fn void inv_sub_bytes(Aes* aes)
{
	for (usz i = 0; i < 4; i++)
	{
		for (usz j = 0; j < 4; j++)
		{
			aes.state[j][i] = get_sbox_invert(aes.state[j][i]);
		}
	}
}

fn void inv_shift_rows(Aes* aes)
{
	char temp;

	temp            = aes.state[3][1];
	aes.state[3][1] = aes.state[2][1];
	aes.state[2][1] = aes.state[1][1];
	aes.state[1][1] = aes.state[0][1];
	aes.state[0][1] = temp;

	temp	        = aes.state[0][2];
	aes.state[0][2] = aes.state[2][2];
	aes.state[2][2] = temp;

	temp	        = aes.state[1][2];
	aes.state[1][2] = aes.state[3][2];
	aes.state[3][2] = temp;

	temp	        = aes.state[0][3];
	aes.state[0][3] = aes.state[1][3];
	aes.state[1][3] = aes.state[2][3];
	aes.state[2][3] = aes.state[3][3];
	aes.state[3][3] = temp;
}

fn void aes_cipher(Aes* aes, char[] round_key)
{
	usz round = 0;
	add_round_key(aes, 0, round_key);

	for LOOP: (round = 1;; round++)
	{
		sub_bytes(aes);
		shift_rows(aes);
		if (round == aes.type.nr) break LOOP;
		mix_columns(aes);
		add_round_key(aes, round, round_key);
	}
	add_round_key(aes, aes.type.nr, round_key);
}

fn void inv_cipher(Aes* aes, char[] round_key)
{
	add_round_key(aes, aes.type.nr, round_key);
	for (usz round = aes.type.nr - 1; ; round--)
	{
		inv_shift_rows(aes);
		inv_sub_bytes(aes);
		add_round_key(aes, round, round_key);
		if (!round) return;
		inv_mix_columns(aes);
	}
}

<*¨
 @param type : "The AES variant to expant the key for"
 @param [inout] round_key : "Key to expand into"
 @param [in] key : "The key to expand"
 @require key.len == type.key.key_len : "Key does not match expected length."
*>
fn void key_expansion(AesType type, char[] key, char[] round_key) @private
{
	usz nk = type.key.nk;
	for (usz i = 0; i < nk; i++)
	{
		round_key[(i * 4) + 0] = key[(i * 4) + 0];
		round_key[(i * 4) + 1] = key[(i * 4) + 1];
		round_key[(i * 4) + 2] = key[(i * 4) + 2];
		round_key[(i * 4) + 3] = key[(i * 4) + 3];
	}

	for (usz i = nk; i < COLNUM * (type.key.nr + 1); i++)
	{
		usz k = (i - 1) * 4;

		char[4] tempa @noinit;

		tempa[0] = round_key[k + 0];
		tempa[1] = round_key[k + 1];
		tempa[2] = round_key[k + 2];
		tempa[3] = round_key[k + 3];

		if (i % nk == 0)
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

			tempa[0] = tempa[0] ^ RCON[i / nk];
		}

		if (type.key.key_size == 256)
		{
			if (i % nk == 4)
			{
				// subword
				tempa[0] = get_sbox_value(tempa[0]);
				tempa[1] = get_sbox_value(tempa[1]);
				tempa[2] = get_sbox_value(tempa[2]);
				tempa[3] = get_sbox_value(tempa[3]);
			}
		}
		usz j = i * 4;
		k = (i - nk) * 4;
		round_key[j + 0] = round_key[k + 0] ^ tempa[0];
		round_key[j + 1] = round_key[k + 1] ^ tempa[1];
		round_key[j + 2] = round_key[k + 2] ^ tempa[2];
		round_key[j + 3] = round_key[k + 3] ^ tempa[3];
	}
}

