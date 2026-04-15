<*
This is an implementation of the AES algorithm with the ECB, CTR and CBC
modes. The key size can be chosen among AES128, AES192, AES256.

Ported from github.com/kokke/tiny-aes-c by Koni Marti.
Constant-time bitsliced SBOX implementation based on BearSSL (bearssl.org)
and the Boyar-Peralta circuit logic.

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
 CTR_LE - WinZip's Counter Mode (Little-endian counter increment)
*>
enum BlockMode
{
	ECB,
	CBC,
	CTR,
	CTR_LE,
}

<* AES type: 128, 192 or 256 bits *>
enum AesType : (AesKey key)
{
	AES128 {{ 128, 16, 176, 4, 10 }},
	AES192 {{ 192, 24, 208, 6, 12 }},
	AES256 {{ 256, 32, 240, 8, 14 }}
}

struct AesKey
{
	<* Size of key in bits *>
	sz key_size;
	<* Size of key in bytes *>
	int key_len;
	<* Size of the expanded round_key *>
	int key_exp_size; // expected size of round_key
	<* Number of 32 bit words in key *>
	sz nk;
	<* Number of rounds in the cipher *>
	sz nr;
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
alias AesState = char[<16>];

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

<* Erases data stored in the context. *>
fn void Aes.destroy(&self)
{
	*self = {};
}


<* Rotate nibbles (columns) forward. *>
fn Type rotl_nibbles(Type w) <Type> @private @inline
{
	$if Type == ulong:
		return ((w << 4) & (ulong)0xFFF0FFF0FFF0FFF0ULL) | ((w >> 12) & (ulong)0x000F000F000F000FULL);
	$else
		return ((w << 4) & (Type)0xFFF0) | ((w >> 12) & (Type)0x000F);
	$endif
}

<* Rotate rows forward within each nibble. *>
fn Type rotl_rows(Type w) <Type> @private @inline
{
	$if Type == ulong:
		return ((w << 1) & (ulong)0xEEEEEEEEEEEEEEEEULL) | ((w >> 3) & (ulong)0x1111111111111111ULL);
	$else
		return ((w << 1) & (Type)0xEEEE) | ((w >> 3) & (Type)0x1111);
	$endif
}

<* Bitsliced S-box. *>
fn void bitsliced_sbox(Type[8]* q) <Type> @private @inline
{
	Type x0, x1, x2, x3, x4, x5, x6, x7;
	Type y1, y2, y3, y4, y5, y6, y7, y8, y9;
	Type y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21;
	Type z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15, z16, z17;
	Type t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19;
	Type t20, t21, t22, t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37, t38, t39;
	Type t40, t41, t42, t43, t44, t45, t46, t47, t48, t49, t50, t51, t52, t53, t54, t55, t56, t57, t58, t59;
	Type t60, t61, t62, t63, t64, t65, t66, t67;
	Type s0, s1, s2, s3, s4, s5, s6, s7;

	x0 = (*q)[7]; x1 = (*q)[6]; x2 = (*q)[5]; x3 = (*q)[4];
	x4 = (*q)[3]; x5 = (*q)[2]; x6 = (*q)[1]; x7 = (*q)[0];

	// Top linear transformation
	y14 = x3 ^ x5; y13 = x0 ^ x6; y9 = x0 ^ x3; y8 = x0 ^ x5;
	t0 = x1 ^ x2; y1 = t0 ^ x7; y4 = y1 ^ x3; y12 = y13 ^ y14;
	y2 = y1 ^ x0; y5 = y1 ^ x6; y3 = y5 ^ y8; t1 = x4 ^ y12;
	y15 = t1 ^ x5; y20 = t1 ^ x1; y6 = y15 ^ x7; y10 = y15 ^ t0;
	y11 = y20 ^ y9; y7 = x7 ^ y11; y17 = y10 ^ y11; y19 = y10 ^ y8;
	y16 = t0 ^ y11; y21 = y13 ^ y16; y18 = x0 ^ y16;

	// Non-linear section
	t2 = y12 & y15; t3 = y3 & y6; t4 = t3 ^ t2; t5 = y4 & x7;
	t6 = t5 ^ t2; t7 = y13 & y16; t8 = y5 & y1; t9 = t8 ^ t7;
	t10 = y2 & y7; t11 = t10 ^ t7; t12 = y9 & y11; t13 = y14 & y17;
	t14 = t13 ^ t12; t15 = y8 & y10; t16 = t15 ^ t12; t17 = t4 ^ t14;
	t18 = t6 ^ t16; t19 = t9 ^ t14; t20 = t11 ^ t16; t21 = t17 ^ y20;
	t22 = t18 ^ y19; t23 = t19 ^ y21; t24 = t20 ^ y18;

	t25 = t21 ^ t22; t26 = t21 & t23; t27 = t24 ^ t26; t28 = t25 & t27;
	t29 = t28 ^ t22; t30 = t23 ^ t24; t31 = t22 ^ t26; t32 = t31 & t30;
	t33 = t32 ^ t24; t34 = t23 ^ t33; t35 = t27 ^ t33; t36 = t24 & t35;
	t37 = t36 ^ t34; t38 = t27 ^ t36; t39 = t29 & t38; t40 = t25 ^ t39;

	t41 = t40 ^ t37; t42 = t29 ^ t33; t43 = t29 ^ t40; t44 = t33 ^ t37;
	t45 = t42 ^ t41; z0 = t44 & y15; z1 = t37 & y6; z2 = t33 & x7;
	z3 = t43 & y16; z4 = t40 & y1; z5 = t29 & y7; z6 = t42 & y11;
	z7 = t45 & y17; z8 = t41 & y10; z9 = t44 & y12; z10 = t37 & y3;
	z11 = t33 & y4; z12 = t43 & y13; z13 = t40 & y5; z14 = t29 & y2;
	z15 = t42 & y9; z16 = t45 & y14; z17 = t41 & y8;

	// Bottom linear transformation
	t46 = z15 ^ z16; t47 = z10 ^ z11; t48 = z5 ^ z13; t49 = z9 ^ z10;
	t50 = z2 ^ z12; t51 = z2 ^ z5; t52 = z7 ^ z8; t53 = z0 ^ z3;
	t54 = z6 ^ z7; t55 = z16 ^ z17; t56 = z12 ^ t48; t57 = t50 ^ t53;
	t58 = z4 ^ t46; t59 = z3 ^ t54; t60 = t46 ^ t57; t61 = z14 ^ t57;
	t62 = t52 ^ t58; t63 = t49 ^ t58; t64 = z4 ^ t59; t65 = t61 ^ t62;
	t66 = z1 ^ t63; s0 = t59 ^ t63; s6 = t56 ^ ~t62; s7 = t48 ^ ~t60;
	t67 = t64 ^ t65; s3 = t53 ^ t66; s4 = t51 ^ t66; s5 = t47 ^ t65;
	s1 = t64 ^ ~s3; s2 = t55 ^ ~t67;

	(*q)[7] = s0; (*q)[6] = s1; (*q)[5] = s2; (*q)[4] = s3;
	(*q)[3] = s4; (*q)[2] = s5; (*q)[1] = s6; (*q)[0] = s7;
}

<* Bitsliced inverse S-box. *>
fn void bitsliced_inv_sbox(Type[8]* q) <Type> @private @inline
{
	Type q0, q1, q2, q3, q4, q5, q6, q7;

	// Pre-transform (inverse affine)
	q0 = ~(*q)[0]; q1 = ~(*q)[1]; q2 = (*q)[2]; q3 = (*q)[3];
	q4 = (*q)[4]; q5 = ~(*q)[5]; q6 = ~(*q)[6]; q7 = (*q)[7];
	(*q)[7] = q1 ^ q4 ^ q6;
	(*q)[6] = q0 ^ q3 ^ q5;
	(*q)[5] = q7 ^ q2 ^ q4;
	(*q)[4] = q6 ^ q1 ^ q3;
	(*q)[3] = q5 ^ q0 ^ q2;
	(*q)[2] = q4 ^ q7 ^ q1;
	(*q)[1] = q3 ^ q6 ^ q0;
	(*q)[0] = q2 ^ q5 ^ q7;

	// Forward S-box
	bitsliced_sbox{Type}(q);

	// Post-transform (inverse affine)
	q0 = (*q)[0]; q1 = (*q)[1]; q2 = (*q)[2]; q3 = (*q)[3];
	q4 = (*q)[4]; q5 = (*q)[5]; q6 = (*q)[6]; q7 = (*q)[7];
	Type r0 = ~q0; Type r1 = ~q1; Type r2 = q2; Type r3 = q3;
	Type r4 = q4; Type r5 = ~q5; Type r6 = ~q6; Type r7 = q7;
	(*q)[7] = r1 ^ r4 ^ r6;
	(*q)[6] = r0 ^ r3 ^ r5;
	(*q)[5] = r7 ^ r2 ^ r4;
	(*q)[4] = r6 ^ r1 ^ r3;
	(*q)[3] = r5 ^ r0 ^ r2;
	(*q)[2] = r4 ^ r7 ^ r1;
	(*q)[1] = r3 ^ r6 ^ r0;
	(*q)[0] = r2 ^ r5 ^ r7;
}

<* Bitsliced ShiftRows. *>
fn void bitsliced_shift_rows(Type[8]* q) <Type> @private @inline
{
	Type m0, m1, m2, m3;
	$if Type == ulong:
		m0 = (ulong)0x1111111111111111ULL; m1 = (ulong)0x2222222222222222ULL;
		m2 = (ulong)0x4444444444444444ULL; m3 = (ulong)0x8888888888888888ULL;
	$else
		m0 = (Type)0x1111; m1 = (Type)0x2222; m2 = (Type)0x4444; m3 = (Type)0x8888;
	$endif

	for (int i = 0; i < 8; i++)
	{
		Type w = (*q)[i];
		// Row 0: no shift
		// Row 1: cyclic shift left 1 = RotL 12 (or RotR 4)
		// Row 2: cyclic shift left 2 = RotL 8
		// Row 3: cyclic shift left 3 = RotL 4
		Type rot4  = rotl_nibbles{Type}(w);
		Type rot8  = rotl_nibbles{Type}(rot4);
		Type rot12 = rotl_nibbles{Type}(rot8);

		(*q)[i] = (w     & m0) |
				 (rot12 & m1) |
				 (rot8  & m2) |
				 (rot4  & m3);
	}
}

<* Bitsliced MixColumns. *>
fn void bitsliced_mix_columns(Type[8]* q) <Type> @private @inline
{
	Type[8] s;
	Type[8] t;
	for (int i = 0; i < 8; i++)
	{
		Type p = (*q)[i];
		// s1 = rot(p, 1) = left shift 1 element = bit 1 -> 0, 2 -> 1, 3 -> 2, 0 -> 3
		// This is rotr(1) = rotl(3)
		Type r3 = rotl_rows{Type}(p);  // rotl 1
		Type r2 = rotl_rows{Type}(r3); // rotl 2
		Type r1 = rotl_rows{Type}(r2); // rotl 3 (this is s1)

		s[i] = p ^ r1 ^ r2 ^ r3;
		t[i] = p ^ r1;
	}

	Type t0 = t[0]; Type t1 = t[1]; Type t2 = t[2]; Type t3 = t[3];
	Type t4 = t[4]; Type t5 = t[5]; Type t6 = t[6]; Type t7 = t[7];

	(*q)[0] = t7 ^ s[0] ^ (*q)[0];
	(*q)[1] = t0 ^ t7 ^ s[1] ^ (*q)[1];
	(*q)[2] = t1 ^ s[2] ^ (*q)[2];
	(*q)[3] = t2 ^ t7 ^ s[3] ^ (*q)[3];
	(*q)[4] = t3 ^ t7 ^ s[4] ^ (*q)[4];
	(*q)[5] = t4 ^ s[5] ^ (*q)[5];
	(*q)[6] = t5 ^ s[6] ^ (*q)[6];
	(*q)[7] = t6 ^ s[7] ^ (*q)[7];
}

macro bool is_valid_encryption_len(BlockMode mode, sz len)
{
	switch (mode)
	{
		case CTR:
		case CTR_LE:
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
		case ECB: ecb_encrypt_buffer(self, in, out);
		case CBC: cbc_encrypt_buffer(self, in, out);
		case CTR: ctr_xcrypt_buffer(self, in, out);
		case CTR_LE: ctr_xcrypt_buffer(self, in, out, &inc_le);
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
		case CTR_LE: ctr_xcrypt_buffer(self, in, out, &inc_le);
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
	char[] out = alloc::alloc_array(allocator, char, in.len);
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
	char[] out = alloc::alloc_array(allocator, char, in.len);
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
	mem::copy(&aes.state, in, 16);
	aes_cipher(aes, &aes.round_key);
	mem::copy(out, &aes.state, 16);
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void ecb_decrypt_block(Aes *aes, char[BLOCKLEN]* in, char[BLOCKLEN]* out)
{
	mem::copy(&aes.state, in, 16);
	inv_cipher(aes, &aes.round_key);
	mem::copy(out, &aes.state, 16);
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
 @require out.len >= in.len : "out must be at least as large as buf"
*>
fn void ecb_decrypt_buffer(Aes *aes, char[] in, char[] out)
{
	sz len = in.len;
	for (sz i = 0; i < len; i += BLOCKLEN)
	{
		ecb_decrypt_block(aes, (char[16]*)&in[i], (char[16]*)&out[i]) @inline;
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void ecb_encrypt_buffer(Aes *aes, char[] in, char[] out)
{
	sz len = in.len;
	sz i = 0;
	// Parallel path for 4 blocks
	for (; i + 64 <= len; i += 64)
	{
		char[64] tmp;
		mem::copy(&tmp, &in[i], 64);
		aes_cipher4(aes, &tmp, &aes.round_key) @inline;
		mem::copy(&out[i], &tmp, 64);
	}
	// Fallback for remaining blocks
	for (; i < len; i += BLOCKLEN)
	{
		ecb_encrypt_block(aes, (char[16]*)&in[i], (char[16]*)&out[i]) @inline;
	}
}

fn void xor_with_iv(char[] buf, char[BLOCKLEN]* iv) @local
{
	char[<16>] v;
	char[<16>] vi;
	mem::copy(&v, buf.ptr, 16);
	mem::copy(&vi, iv, 16);
	v ^= vi;
	mem::copy(buf.ptr, &v, 16);
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext input."
 @param [out] out : "Cipher output."
*>
fn void cbc_encrypt_buffer(Aes *aes, char[] in, char[] out)
{
	char[BLOCKLEN] iv = aes.iv;
	sz len = in.len;
	for (sz i = 0; i < len; i += BLOCKLEN)
	{
		char[BLOCKLEN] tmp = in[i:BLOCKLEN];
		xor_with_iv(tmp[..], (char[16]*)&iv);
		ecb_encrypt_block(aes, (char[16]*)&tmp, (char[16]*)&out[i]);
		iv = out[i:BLOCKLEN];
	}
}

<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Cipher input."
 @param [out] out : "Plaintext output."
*>
fn void cbc_decrypt_buffer(Aes *aes, char[] in, char[] out)
{
	sz len = in.len;
	for (sz i = 0; i < len; i += BLOCKLEN)
	{
		char[BLOCKLEN] tmp;
		ecb_decrypt_block(aes, in[i:BLOCKLEN], (char[16]*)&tmp);
		xor_with_iv(tmp[..], (char[16]*)&aes.iv);
		aes.iv = in[i:BLOCKLEN];
		out[i:BLOCKLEN] = tmp[..];
	}
}

alias BlckCounter = fn void(char[]);

fn void inc_be(char[] iv)
{
	for (int bi = (BLOCKLEN - 1); bi >= 0; bi--)
	{
		if (iv[bi] == 255)
		{
			iv[bi] = 0;
			continue;
		}
		iv[bi]++;
		break;
	}
}

fn void inc_le(char[] iv)
{
	for (int bi = 0; bi < BLOCKLEN; bi++)
	{
		if (iv[bi] == 255)
		{
			iv[bi] = 0;
			continue;
		}
		iv[bi]++;
		break;
	}
}


<*
 @param [&inout] aes : "AES context."
 @param [in] in : "Plaintext/cipher input."
 @param [out] out : "Cipher/plaintext output."
*>
fn void ctr_xcrypt_buffer(Aes *aes, char[] in, char[] out, BlckCounter inc = &inc_be)
{
	 char[BLOCKLEN] buffer @noinit;
	 sz len = in.len;
	 sz i = 0;

	 // Parallel path for 4 blocks
	 for (; i + 64 <= len; i += 64)
	 {
		 char[64] counters;
		 for (int b = 0; b < 4; b++)
		 {
			 mem::copy(&counters[b * 16], &aes.iv, 16);
			 // Increment IV for next block
			 inc(aes.iv[:16]);
		 }
		 aes_cipher4(aes, &counters, &aes.round_key) @inline;
		 char[<64>] c4;
		 char[<64>] i4;
		 mem::copy(&c4, &counters, 64);
		 mem::copy(&i4, &in[i], 64);
		 c4 ^= i4;
		 mem::copy(&out[i], &c4, 64);
	 }

	 for (int bi = BLOCKLEN; i < len; i++)
	 {
		 if (bi == BLOCKLEN)
		 {
			 buffer = aes.iv;
			 ecb_encrypt_block(aes, &buffer, &buffer);
			 inc(aes.iv[..]);
			 bi = 0;
		 }
		 out[i] = in[i] ^ buffer[bi];
		 bi++;
	 }
}

fn void transpose(char[16]* in, ushort[8]* q) @private @inline
{
	ushort q0, q1, q2, q3, q4, q5, q6, q7;
	for (int i = 0; i < 16; i++)
	{
		char b = (*in)[i];
		q0 |= (ushort)((b >> 0) & 1u) << i;
		q1 |= (ushort)((b >> 1) & 1u) << i;
		q2 |= (ushort)((b >> 2) & 1u) << i;
		q3 |= (ushort)((b >> 3) & 1u) << i;
		q4 |= (ushort)((b >> 4) & 1u) << i;
		q5 |= (ushort)((b >> 5) & 1u) << i;
		q6 |= (ushort)((b >> 6) & 1u) << i;
		q7 |= (ushort)((b >> 7) & 1u) << i;
	}
	(*q)[0] = q0; (*q)[1] = q1; (*q)[2] = q2; (*q)[3] = q3;
	(*q)[4] = q4; (*q)[5] = q5; (*q)[6] = q6; (*q)[7] = q7;
}

fn void untranspose(ushort[8]* q, char[16]* out) @private @inline
{
	ushort q0 = (*q)[0]; ushort q1 = (*q)[1]; ushort q2 = (*q)[2]; ushort q3 = (*q)[3];
	ushort q4 = (*q)[4]; ushort q5 = (*q)[5]; ushort q6 = (*q)[6]; ushort q7 = (*q)[7];
	for (int i = 0; i < 16; i++)
	{
		char b = (char)((q0 >> i) & 1u) << 0;
		b |= (char)((q1 >> i) & 1u) << 1;
		b |= (char)((q2 >> i) & 1u) << 2;
		b |= (char)((q3 >> i) & 1u) << 3;
		b |= (char)((q4 >> i) & 1u) << 4;
		b |= (char)((q5 >> i) & 1u) << 5;
		b |= (char)((q6 >> i) & 1u) << 6;
		b |= (char)((q7 >> i) & 1u) << 7;
		(*out)[i] = b;
	}
}

fn void sub_bytes(Aes* aes) @inline
{
	ushort[8] q;
	transpose((char[16]*)&aes.state, &q);
	bitsliced_sbox{ushort}(&q);
	untranspose(&q, (char[16]*)&aes.state);
}

fn void inv_sub_bytes(Aes* aes) @inline
{
	ushort[8] q;
	transpose((char[16]*)&aes.state, &q);
	bitsliced_inv_sbox{ushort}(&q);
	untranspose(&q, (char[16]*)&aes.state);
}

fn void sub_word_ct(char[4]* word) @private
{
	ushort[8] q;
	char[16] in;
	mem::set(&in, 0, 16);
	mem::copy(&in, word, 4);
	transpose(&in, &q);
	bitsliced_sbox{ushort}(&q);
	untranspose(&q, &in);
	mem::copy(word, &in, 4);
}

fn void transpose4(char[64]* in, ulong[8]* q) @private @inline
{
	mem::set(q, 0, 8 * ulong::size);
	for (int i = 0; i < 64; i++)
	{
		char v = (*in)[i];
		for (int j = 0; j < 8; j++)
		{
			(*q)[j] |= (ulong)((v >> j) & 1u) << i;
		}
	}
}

fn void untranspose4(ulong[8]* q, char[64]* out) @private @inline
{
	mem::set(out, 0, 64);
	for (int i = 0; i < 64; i++)
	{
		char v = 0;
		for (int j = 0; j < 8; j++)
		{
			v |= (char)(((*q)[j] >> i) & 1u) << j;
		}
		(*out)[i] = v;
	}
}

fn void add_round_key_bitsliced(ulong[8]* q, char[] round_key, sz round) @private @inline
{
	ushort[8] kq;
	transpose((char[16]*)&round_key[round * 16], &kq);
	for (int i = 0; i < 8; i++)
	{
		ulong k = (ulong)kq[i];
		ulong k4 = k | (k << 16) | (k << 32) | (k << 48);
		(*q)[i] ^= k4;
	}
}

fn void aes_cipher4(Aes* aes, char[64]* data, char[] round_key) @private
{
	ulong[8] q;
	transpose4(data, &q);

	add_round_key_bitsliced(&q, round_key, 0);

	for (sz r = 1; r < aes.type.nr; r++)
	{
		bitsliced_sbox{ulong}(&q);
		bitsliced_shift_rows{ulong}(&q);
		bitsliced_mix_columns{ulong}(&q);
		add_round_key_bitsliced(&q, round_key, r);
	}

	bitsliced_sbox{ulong}(&q);
	bitsliced_shift_rows{ulong}(&q);
	add_round_key_bitsliced(&q, round_key, aes.type.nr);

	untranspose4(&q, data);
}

const char[11] RCON = x`8d01020408102040801b36`;

fn void add_round_key(Aes* aes, sz round, char[] round_key)
{
	char[<16>] k;
	mem::copy(&k, &round_key[round * 16], 16);
	aes.state ^= k;
}

fn void shift_rows(Aes* aes)
{
	char[<16>] s = aes.state;
	aes.state = {
		s[0],  s[5],  s[10], s[15],
		s[4],  s[9],  s[14], s[3],
		s[8],  s[13], s[2],  s[7],
		s[12], s[1],  s[6],  s[11]
	};
}

fn char[<16>] xtime_vec(char[<16>] x) @local @inline
{
	return (x << 1) ^ (((x >> 7) & 1u) * 0x1b);
}

fn void mix_columns(Aes* aes)
{
	char[<16>] s = aes.state;
	char[<16>] s1 = {s[1], s[2], s[3], s[0], s[5], s[6], s[7], s[4], s[9], s[10], s[11], s[8], s[13], s[14], s[15], s[12]};
	char[<16>] s2 = {s[2], s[3], s[0], s[1], s[6], s[7], s[4], s[5], s[10], s[11], s[8], s[9], s[14], s[15], s[12], s[13]};
	char[<16>] s3 = {s[3], s[0], s[1], s[2], s[7], s[4], s[5], s[6], s[11], s[8], s[9], s[10], s[15], s[12], s[13], s[14]};

	char[<16>] tmp = s ^ s1 ^ s2 ^ s3;

	aes.state ^= xtime_vec(s ^ s1) ^ tmp;
}

fn char xtime(char x) @local
{
	return ((x << 1) ^ (((x >> 7) & 1u) * 0x1b));
}

fn char multiply(char x, char y) @local
{
	return (((y & 1u) * x) ^
		   (((y>>1) & 1u) * xtime(x)) ^
		   (((y>>2) & 1u) * xtime(xtime(x))) ^
		   (((y>>3) & 1u) * xtime(xtime(xtime(x)))) ^
		   (((y>>4) & 1u) * xtime(xtime(xtime(xtime(x))))));
}

fn void inv_mix_columns(Aes* aes)
{
	for (int i = 0; i < 4; i++)
	{
		char a = aes.state[i * 4 + 0];
		char b = aes.state[i * 4 + 1];
		char c = aes.state[i * 4 + 2];
		char d = aes.state[i * 4 + 3];

		aes.state[i * 4 + 0] = multiply(a, 0x0e) ^ multiply(b, 0x0b) ^ multiply(c, 0x0d) ^ multiply(d, 0x09);
		aes.state[i * 4 + 1] = multiply(a, 0x09) ^ multiply(b, 0x0e) ^ multiply(c, 0x0b) ^ multiply(d, 0x0d);
		aes.state[i * 4 + 2] = multiply(a, 0x0d) ^ multiply(b, 0x09) ^ multiply(c, 0x0e) ^ multiply(d, 0x0b);
		aes.state[i * 4 + 3] = multiply(a, 0x0b) ^ multiply(b, 0x0d) ^ multiply(c, 0x09) ^ multiply(d, 0x0e);
	}
}

fn void inv_shift_rows(Aes* aes)
{
	char[<16>] s = aes.state;
	aes.state = {
		s[0],  s[13], s[10], s[7],
		s[4],  s[1],  s[14], s[11],
		s[8],  s[5],  s[2],  s[15],
		s[12], s[9],  s[6],  s[3]
	};
}

fn void aes_cipher(Aes* aes, char[] round_key)
{
	sz round = 0;
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
	for (sz round = aes.type.nr - 1; ; round--)
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
	sz nk = type.key.nk;
	for (sz i = 0; i < nk; i++)
	{
		round_key[(i * 4) + 0] = key[(i * 4) + 0];
		round_key[(i * 4) + 1] = key[(i * 4) + 1];
		round_key[(i * 4) + 2] = key[(i * 4) + 2];
		round_key[(i * 4) + 3] = key[(i * 4) + 3];
	}

	for (sz i = nk; i < COLNUM * (type.key.nr + 1); i++)
	{
		sz k = (i - 1) * 4;

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
			sub_word_ct(&tempa);

			tempa[0] = tempa[0] ^ RCON[i / nk];
		}

		if (type.key.key_size == 256)
		{
			if (i % nk == 4)
			{
				// subword
				sub_word_ct(&tempa);
			}
		}
		sz j = i * 4;
		k = (i - nk) * 4;
		round_key[j + 0] = round_key[k + 0] ^ tempa[0];
		round_key[j + 1] = round_key[k + 1] ^ tempa[1];
		round_key[j + 2] = round_key[k + 2] ^ tempa[2];
		round_key[j + 3] = round_key[k + 3] ^ tempa[3];
	}
}
