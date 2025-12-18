// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// ChaCha20 code dedicated from repo: https://github.com/NotsoanoNimus/chacha20_aead.c3l (but massively cleaned)
module chacha20_tests @test;

import std::crypto::chacha20;
import std::hash::sha256;
import std::collections;
import std::math;


<* Common encrypted content used for some basic tests. *>
const char[] TEST_BLOB = x'38fd7763067906f644509fe87f2efc9168a92344dc33720f0797181b1b01e935b90c230968187680f7b5e1496b675655ee293f62f7cceeaa4dc710649fab066ef48944f1379e0365cf4132512b537bbb04704b408b82f9bfcee55ef00c6890a35479d3f83e79b8274a8e21747b775cb5d8c24a5a314dea5520513b62248e0f5e05bffa0bf2930aadaf56359f33ab011f534b';

fn void encrypt_clone() => @pool()
{
	// Test values. Note that the key is exactly 256 bits and the nonce is exactly 12 bytes.
	char[*] key = "this is a test key for chacha20.";
	char[*] nonce = "1234567890ab";
	uint counter = 1;

	// Need to copy this string into a buffer that's R/W. Trying to modify this buffer in-place is a recipe for disaster.
	char[] constant =
		"this is a stream of data which should be encrypted with chacha20."
		" It's not authenticated yet, but it works to hide valuable data cost-effectively.";

	char[] result = chacha20::tencrypt(constant, key, nonce, counter);

	test::@check(result.ptr != constant.ptr, "The cloned buffer's pointer should NOT equal the const buffer's pointer.");
	test::@check(result.len == constant.len, "`encrypt_clone` should only encrypt and clone the exact length of the const string buffer (got '%s'; expected '%s').", result.len, constant.len);
	test::@check(result == TEST_BLOB, "ChaCha20 encryption failed: mismatch on expected ciphertext.");
}

fn void encrypt_mut__streamed()
{
	char[*] key = "this is a test key for chacha20.";
	char[*] nonce = "1234567890ab";
	uint counter = 1;

	char[] constant = {
		...
		"this is a stream of data which should be encrypted with chacha20."
		" It's not authenticated yet, but it works to hide valuable data cost-effectively."
	};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(constant[:5]);
	c.transform(constant[5:5]);
	c.transform(constant[10..]);

	test::@check(constant == TEST_BLOB, "ChaCha20 encryption failed: mismatch on expected ciphertext.");
}


fn void decrypt_mut()
{
	char[*] key = "this is a test key for chacha20.";
	char[*] nonce = "1234567890ab";
	uint counter = 1;

	char[] constant =
		"this is a stream of data which should be encrypted with chacha20."
		" It's not authenticated yet, but it works to hide valuable data cost-effectively.";

	// This input should decrypt to the expected plaintext above.
	char[] input_enc = { ...TEST_BLOB }; // made mutable with splat
	chacha20::decrypt_mut(input_enc, key, nonce, counter);
	test::@check(input_enc == constant, "ChaCha20 decryption failed: mismatch on expected plaintext.");

	// Replaced the first 4 bytes (uint) with 0. This should still decrypt, but must fail.
	char[] bad_input_enc = { 0, 0, 0, 0, ...TEST_BLOB[4..] };
	chacha20::decrypt_mut(bad_input_enc, key, nonce, counter);
	test::@check(bad_input_enc != constant, "ChaCha20 decryption succeeded when it should not have.");
}


fn void rfc8439_apx_1__vector1()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	uint counter = 0;
	char[64] arr = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(arr[..]);

	uint[] expected_state = {
		0xade0b876, 0x903df1a0, 0xe56a5d40, 0x28bd8653,
		0xb819d2bd, 0x1aed8da0, 0xccef36a8, 0xc70d778b,
		0x7c5941da, 0x8d485751, 0x3fe02477, 0x374ad8b8,
		0xf4b8436a, 0x1ca11815, 0x69b687c3, 0x8665eeb2,
	};
	test::@check(c.key_stream[..] == expected_state, "Expected ChaCha20 state mismatch.");
}


fn void rfc8439_apx_1__vector2()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	uint counter = 1;
	char[64] arr = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(arr[..]);

	uint[] expected_state = {
		0xbee7079f, 0x7a385155, 0x7c97ba98, 0x0d082d73,
		0xa0290fcb, 0x6965e348, 0x3e53c612, 0xed7aee32,
		0x7621b729, 0x434ee69c, 0xb03371d5, 0xd539d874,
		0x281fed31, 0x45fb0a51, 0x1f0ae1ac, 0x6f4d794b,
	};
	test::@check(c.key_stream[..] == expected_state, "Expected ChaCha20 state mismatch.");
}


fn void rfc8439_apx_1__vector3()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	uint counter = 1;
	char[64] arr = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(arr[..]);

	uint[] expected_state = {
		0x2452eb3a, 0x9249f8ec, 0x8d829d9b, 0xddd4ceb1,
		0xe8252083, 0x60818b01, 0xf38422b8, 0x5aaa49c9,
		0xbb00ca8e, 0xda3ba7b4, 0xc4b592d1, 0xfdf2732f,
		0x4436274e, 0x2561b3c8, 0xebdd4aa6, 0xa0136c00,
	};
	test::@check(c.key_stream[..] == expected_state, "Expected ChaCha20 state mismatch.");
}


fn void rfc8439_apx_1__vector4()
{
	char[*] key = { 0, 0xff, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	uint counter = 2;
	char[64] arr = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(arr[..]);

	uint[] expected_state = {
		0xfb4dd572, 0x4bc42ef1, 0xdf922636, 0x327f1394,
		0xa78dea8f, 0x5e269039, 0xa1bebbc1, 0xcaf09aae,
		0xa25ab213, 0x48a6b46c, 0x1b9d9bcb, 0x092c5be6,
		0x546ca624, 0x1bec45d5, 0x87f47473, 0x96f0992e,
	};
	test::@check(c.key_stream[..] == expected_state, "Expected ChaCha20 state mismatch.");
}


fn void rfc8439_apx_1__vector5()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 };
	uint counter = 0;

	char[64] arr = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(arr[..]);

	uint[] expected_state = {
		0x374dc6c2, 0x3736d58c, 0xb904e24a, 0xcd3f93ef,
		0x88228b1a, 0x96a4dfb3, 0x5b76ab72, 0xc727ee54,
		0x0e0e978a, 0xf3145c95, 0x1b748ea8, 0xf786c297,
		0x99c28f5f, 0x628314e8, 0x398a19fa, 0x6ded1b53,
	};
	test::@check(c.key_stream[..] == expected_state, "Expected ChaCha20 state mismatch.");
}


fn void rfc8439_apx_2__vector1()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
	uint counter = 0;

	char[64] plaintext = {};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(plaintext[..]);

	char[] expected_ciphertext = {
		0x76, 0xb8, 0xe0, 0xad, 0xa0, 0xf1, 0x3d, 0x90, 0x40, 0x5d, 0x6a, 0xe5, 0x53, 0x86, 0xbd, 0x28,
		0xbd, 0xd2, 0x19, 0xb8, 0xa0, 0x8d, 0xed, 0x1a, 0xa8, 0x36, 0xef, 0xcc, 0x8b, 0x77, 0x0d, 0xc7,
		0xda, 0x41, 0x59, 0x7c, 0x51, 0x57, 0x48, 0x8d, 0x77, 0x24, 0xe0, 0x3f, 0xb8, 0xd8, 0x4a, 0x37,
		0x6a, 0x43, 0xb8, 0xf4, 0x15, 0x18, 0xa1, 0x1c, 0xc3, 0x87, 0xb6, 0x69, 0xb2, 0xee, 0x65, 0x86,
	};
	test::@check(plaintext[..] == expected_ciphertext, "Expected ChaCha20 ciphertext mismatch.");
}


fn void rfc8439_apx_2__vector2()
{
	char[*] key = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 };
	uint counter = 1;

	char[] plaintext = {
		...
		`Any submission to the IETF intended by the Contributor for publication as all or part of an `
		`IETF Internet-Draft or RFC and any statement made within the context of an IETF activity is `
		`considered an "IETF Contribution". Such statements include oral statements in IETF sessions, `
		`as well as written and electronic communications made at any time or place, which are addressed to`
	};

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(plaintext);

	char[] expected_ciphertext = {
		0xa3, 0xfb, 0xf0, 0x7d, 0xf3, 0xfa, 0x2f, 0xde, 0x4f, 0x37, 0x6c, 0xa2, 0x3e, 0x82, 0x73, 0x70,
		0x41, 0x60, 0x5d, 0x9f, 0x4f, 0x4f, 0x57, 0xbd, 0x8c, 0xff, 0x2c, 0x1d, 0x4b, 0x79, 0x55, 0xec,
		0x2a, 0x97, 0x94, 0x8b, 0xd3, 0x72, 0x29, 0x15, 0xc8, 0xf3, 0xd3, 0x37, 0xf7, 0xd3, 0x70, 0x05,
		0x0e, 0x9e, 0x96, 0xd6, 0x47, 0xb7, 0xc3, 0x9f, 0x56, 0xe0, 0x31, 0xca, 0x5e, 0xb6, 0x25, 0x0d,
		0x40, 0x42, 0xe0, 0x27, 0x85, 0xec, 0xec, 0xfa, 0x4b, 0x4b, 0xb5, 0xe8, 0xea, 0xd0, 0x44, 0x0e,
		0x20, 0xb6, 0xe8, 0xdb, 0x09, 0xd8, 0x81, 0xa7, 0xc6, 0x13, 0x2f, 0x42, 0x0e, 0x52, 0x79, 0x50,
		0x42, 0xbd, 0xfa, 0x77, 0x73, 0xd8, 0xa9, 0x05, 0x14, 0x47, 0xb3, 0x29, 0x1c, 0xe1, 0x41, 0x1c,
		0x68, 0x04, 0x65, 0x55, 0x2a, 0xa6, 0xc4, 0x05, 0xb7, 0x76, 0x4d, 0x5e, 0x87, 0xbe, 0xa8, 0x5a,
		0xd0, 0x0f, 0x84, 0x49, 0xed, 0x8f, 0x72, 0xd0, 0xd6, 0x62, 0xab, 0x05, 0x26, 0x91, 0xca, 0x66,
		0x42, 0x4b, 0xc8, 0x6d, 0x2d, 0xf8, 0x0e, 0xa4, 0x1f, 0x43, 0xab, 0xf9, 0x37, 0xd3, 0x25, 0x9d,
		0xc4, 0xb2, 0xd0, 0xdf, 0xb4, 0x8a, 0x6c, 0x91, 0x39, 0xdd, 0xd7, 0xf7, 0x69, 0x66, 0xe9, 0x28,
		0xe6, 0x35, 0x55, 0x3b, 0xa7, 0x6c, 0x5c, 0x87, 0x9d, 0x7b, 0x35, 0xd4, 0x9e, 0xb2, 0xe6, 0x2b,
		0x08, 0x71, 0xcd, 0xac, 0x63, 0x89, 0x39, 0xe2, 0x5e, 0x8a, 0x1e, 0x0e, 0xf9, 0xd5, 0x28, 0x0f,
		0xa8, 0xca, 0x32, 0x8b, 0x35, 0x1c, 0x3c, 0x76, 0x59, 0x89, 0xcb, 0xcf, 0x3d, 0xaa, 0x8b, 0x6c,
		0xcc, 0x3a, 0xaf, 0x9f, 0x39, 0x79, 0xc9, 0x2b, 0x37, 0x20, 0xfc, 0x88, 0xdc, 0x95, 0xed, 0x84,
		0xa1, 0xbe, 0x05, 0x9c, 0x64, 0x99, 0xb9, 0xfd, 0xa2, 0x36, 0xe7, 0xe8, 0x18, 0xb0, 0x4b, 0x0b,
		0xc3, 0x9c, 0x1e, 0x87, 0x6b, 0x19, 0x3b, 0xfe, 0x55, 0x69, 0x75, 0x3f, 0x88, 0x12, 0x8c, 0xc0,
		0x8a, 0xaa, 0x9b, 0x63, 0xd1, 0xa1, 0x6f, 0x80, 0xef, 0x25, 0x54, 0xd7, 0x18, 0x9c, 0x41, 0x1f,
		0x58, 0x69, 0xca, 0x52, 0xc5, 0xb8, 0x3f, 0xa3, 0x6f, 0xf2, 0x16, 0xb9, 0xc1, 0xd3, 0x00, 0x62,
		0xbe, 0xbc, 0xfd, 0x2d, 0xc5, 0xbc, 0xe0, 0x91, 0x19, 0x34, 0xfd, 0xa7, 0x9a, 0x86, 0xf6, 0xe6,
		0x98, 0xce, 0xd7, 0x59, 0xc3, 0xff, 0x9b, 0x64, 0x77, 0x33, 0x8f, 0x3d, 0xa4, 0xf9, 0xcd, 0x85,
		0x14, 0xea, 0x99, 0x82, 0xcc, 0xaf, 0xb3, 0x41, 0xb2, 0x38, 0x4d, 0xd9, 0x02, 0xf3, 0xd1, 0xab,
		0x7a, 0xc6, 0x1d, 0xd2, 0x9c, 0x6f, 0x21, 0xba, 0x5b, 0x86, 0x2f, 0x37, 0x30, 0xe3, 0x7c, 0xfd,
		0xc4, 0xfd, 0x80, 0x6c, 0x22, 0xf2, 0x21  
	};
	test::@check(plaintext == expected_ciphertext, "Expected ChaCha20 ciphertext mismatch.");
}


fn void rfc8439_apx_2__vector3()
{
	char[*] key = {
		0x1c, 0x92, 0x40, 0xa5, 0xeb, 0x55, 0xd3, 0x8a, 0xf3, 0x33, 0x88, 0x86, 0x04, 0xf6, 0xb5, 0xf0,
		0x47, 0x39, 0x17, 0xc1, 0x40, 0x2b, 0x80, 0x09, 0x9d, 0xca, 0x5c, 0xbc, 0x20, 0x70, 0x75, 0xc0,
	};
	char[*] nonce = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2 };
	uint counter = 42;
	char[] plaintext = { ..."'Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe." };

	ChaCha20 c @noinit;
	c.init(key, nonce, counter);
	c.transform(plaintext[..]);

	char[] expected_ciphertext = {
		0x62, 0xe6, 0x34, 0x7f, 0x95, 0xed, 0x87, 0xa4, 0x5f, 0xfa, 0xe7, 0x42, 0x6f, 0x27, 0xa1, 0xdf,
		0x5f, 0xb6, 0x91, 0x10, 0x04, 0x4c, 0x0d, 0x73, 0x11, 0x8e, 0xff, 0xa9, 0x5b, 0x01, 0xe5, 0xcf,
		0x16, 0x6d, 0x3d, 0xf2, 0xd7, 0x21, 0xca, 0xf9, 0xb2, 0x1e, 0x5f, 0xb1, 0x4c, 0x61, 0x68, 0x71,
		0xfd, 0x84, 0xc5, 0x4f, 0x9d, 0x65, 0xb2, 0x83, 0x19, 0x6c, 0x7f, 0xe4, 0xf6, 0x05, 0x53, 0xeb,
		0xf3, 0x9c, 0x64, 0x02, 0xc4, 0x22, 0x34, 0xe3, 0x2a, 0x35, 0x6b, 0x3e, 0x76, 0x43, 0x12, 0xa6,
		0x1a, 0x55, 0x32, 0x05, 0x57, 0x16, 0xea, 0xd6, 0x96, 0x25, 0x68, 0xf8, 0x7d, 0x3f, 0x3f, 0x77,
		0x04, 0xc6, 0xa8, 0xd1, 0xbc, 0xd1, 0xbf, 0x4d, 0x50, 0xd6, 0x15, 0x4b, 0x6d, 0xa7, 0x31, 0xb1,
		0x87, 0xb5, 0x8d, 0xfd, 0x72, 0x8a, 0xfa, 0x36, 0x75, 0x7a, 0x79, 0x7a, 0xc1, 0x88, 0xd1
	};
	test::@check(plaintext[..] == expected_ciphertext, "Expected ChaCha20 ciphertext mismatch.");
}


// Further tests borrowed from BoringSSL:
//   https://github.com/google/boringssl/blob/main/crypto/chacha/chacha_test.cc
//
const char[*] K_KEY = x'98bef1469be7269837a45bfbc92a5a6ac762507cf96443bf33b96b1bd4c6f8f6';
const char[*] K_NONCE = x'44e792d63335abb1582e9253';
const uint K_COUNTER = 42;
const char[] K_INPUT =
	x'5828d530362c605529f8e18cae151526f23a73a0f312a3885f2b74233dc90523c654491e448814d9da3715dcb7e423b39d7e166835fc026dcc8ae5dd5fe4d2566f129c9c7d6a3848'
	x'bddfd9ac1ba24dc543043cd799e1a7139c51c26df9cf073be4bf93a3a9b4c5f01ae48d5fc6c47c697ade1ac1c9cfc24e7a252c32e917ba68f1375d628446f5807f1a71f7be724bb8'
	x'1cfe3ebdae0d730d874a31c33d466fb3d76be3b870178e7a6a0ebfa8bc2bdbfa4fb62620ee63f06d26ac6a18376e5981d160e640d56d68ba8b654af9f1ae56248fe38ee77e6fcf92'
	x'dfa9753ad62e1caff2d68b39add25dfbd7df05570df7f68f2d14b04e1a3c7704cd3c5c5852106fcf5c03c85f852b058260dacccdd688bfc010b36f545442bc4b77214dee8745064c'
	x'6038d27e1d306c55f038801cde3dea683ef63e59cf0d08ae8c020bc1726ab46df3f7b3ef3ab106f2f4d6697b3ea216313179b633a9ca8aa8bef3e93828d1e13b4e2e4735a461141e'
	x'422c4955eae3b3ce39d3b3ef4a4d7849bdf67c0a2cd326cbd96aad6393a72992dc1faf61828074b29c4a867350d8d1ffee1ae2dda261bd10c35f679f29e4d370e5673ad22000cc25'
	x'1596544585ed82883b9f3bc304d423b10ddcc8269d28b3254d52e533f3ed2cb81acfc352b42fc77996147d72277285ea6d41a022136d0683a4dd0f69d201cdc6b8645c2c79d1c7d3'
	x'31db2cffdad06931ad835fed6a97e40043b02e97ae005f5cb9e8398010ca0cfaf0b5cdaa271160d9218693919f2d1a8ede0bb5cb052430454d1175fde5a0a94e3a8c3b525a371805'
	x'4a7a096ae6d5a9a671474c50e13e8a212b4f0ee3cb72c5283e5a33ec48922ea12457090f01853b34397ec79062e2dc5d0a2c5126953a9592a5398f0c830b9d38ab982ac401c40d77'
	x'13cbcaf128315275272cf00486c8f33df29d8f5552403faa227fe7693bee4409deffb0693aae74e99d33ae8b6d6004ff533f88e9639bb16d2c22155a15d9e5cb03783cca598cc8c2'
	x'86ffd279d6c6ec5bbba0ae0120092e385dda5de0594ee58b848fb6e0569f21a1cfb20f2c93f8cf37c19f3298216552666ed3719855b9469f1a35c4476962704b779ee421e6325a26'
	x'05ba5753d79b553cbb5379609cc84df7f51d540291680eaaca5a780c289ac3ac49c0f485ee59767e284ef15c63f7ce0e2c21a058e901fdebd1afe6ef93b3955160a2744015e5f40a'
	x'ca6d9a37424d5a58490fe902fc77d859deddad4b992e6473ad422ff32c0d49e42e6ca47375181485bb64b4a1b06e01c0cf179cc528c32d6c172a3d065cf3b44975ad1769d4ca65ae'
	x'4471a5f60d0f8e37c743ce6b08e9d134488fc9fcf35d2dec62d3f0b3fe2e40557654c7b46116cc7c1c1924e64dd4c377671f3c7479a1f885881d6fa47e2c219f49f5aa4ef34afa9d'
	x'bef6cedab5ab39bd1641a94aac0901ca'
;
const char[] K_OUTPUT =
	x'54306a13da596b6d5949c8c5ab26d48aadc03daf14b915b8cadf17a703d3c50601ef21dda30b9e48b85e0b879f9523688569d25daf57e927113d49faf108cc15ec1d1916129bc866'
	x'1ffa2c93f4991127310ed8464740117001cae85bc591c83adcaaf34b80e5bc03d08972bcce2a760cf5da4c10063541b1e6b4aa7aeff0624ac59f2cafb82fd9d1017a362f3e83a5eb'
	x'8170a0571746ea9ecb0e74d344571d4006f8b7cb5ff479bd1119d6eef8b0aadd0062ad3b889a885b1b07c9ae9ea694e555db4523b92ccd29d354c3881e5f52f2090026261aedf5c2'
	x'a97df9215aaf6dab8e168496b54fcf1ea3af089f7986c3be0c70cb8ff3c5f8e84b217d18a9ed8bfb6b5a6f260b56047cfe0e1ec13f82c573bd530cf0e2c9f33d1b6dba70c16db600'
	x'28e1c4786204da2386c3da743d7cd67629b2272eb235426082cf302c59e4e3d0741f58e8da4745731c0593ae75be1f81d8b7b3fffc8b529eed8b379fe0b8a266e16ac51f1df0de3f'
	x'3db028f3aa4e4d31b026792b080fe92f79b3c8dda789a8a81d590e4f1e931f707f4e7efeb8ca63e0a605ccd7de2a4931785c5f44b29b91991429630912dd02d97be9f51207d0e7e6'
	x'e8dddaa473c48ebd7bb7bbcb832f43f61c50ae9b2e52801885a823527a6af74236ca915a3d2aa0357d70fc4c187c5772cf9b29d6d0b4d7e6897069225e45094d4987845f8a5fe415'
	x'd3e372afb2309cc1ff8e6d2a769e08037ee0c3c297066b332b08e3d50bd832676110ed6bed50efd71c1be06da16419342fe4e854bf840edf0e8bd8dd7796b854abf295590d0d0a15'
	x'6e01f224aba0d8df38ea97587688beaf45e3564f68e84be72b22189682892534d1dd08ea7e21ef575543f7faca1cde992e8bd8c3cf894dfc3b7d4ac999c431b67aaef849b246c160'
	x'0575f33df2c984a4b98a872a875c0abc517d9af5c9242d5ee6c6e3cd7ee4af8a6c0004c8d7a5adfab2084a269b7cd0c613b1b9653f7030f9989d879957713eb1c324f0a6a2609d66'
	x'd25faee39487ead1ea0d2a77ef31ccebf90cdc9c1280bbb08eab9a04cd4b954f7a0b537c16cc0eb17310ddaa769490d98b664131ed8c7d74c433fac3438d10bc844d0e9532df1743'
	x'6dd25e12b9ed33d9976f4acdc3cd8134be7ea2d0a7915d90f65e4a250fcc24ebe1e4626c8f4536975dda202b86008c94a96a69b2e9bb828e4195b4b7f15552303948b32582a91027'
	x'89b5e51fab723c7008cee661bf19c8902b29303eb84c33f0f0152eb725ca994b6f4b4150ee5699cf2ba4c47c5ca6d467045c5d5f269e0fe258684c30cdef46db376fbbc480ca8a54'
	x'5d719d0ce8b82c109044a4883fbc153cd2ca0ec3e46eefb0cbfd617c27f225ea716df7499c8127f06133cf5568d373a4ed35652af23ecf9098546d956a0c9c240eb4b79b8d6e1cbc'
	x'eb171086da916d894cebf5508f40cf4a'
;
const uint K_OVERFLOW_COUNTER = 0xffff_ffff;
const char[] K_OVERFLOW_OUTPUT =
	x'376438cb25692cf5888afe6d3b10073c77accd1c0ca717311dc381d1a52055ead300c984dee2e55e7b282859733a8e576218505597ca503e8a8461284c229350487e6578065acd2b'
	x'11f710fd6f4192827c3a710767d07eb7dfdcfceee655dd6f7923f3aeb12196beea0e1b580b3f6351d4ce98fe1ac7a7437f0ce862cf783f4e31bf2b7691cd19800d7f118b76ef433c'
	x'4f6186c564a8c273c26439a08be67ff626d4474fe446e2f59ee6c7766ca90f1d1b22a5620a883e8cf0bc4c113f0df785670b4ca33fa8f12a652e0003c9499148b7c829282f468e8b'
	x'd67319063e6f92c83d3f4d68bc02c08f71460d2863fead148104b723fd210af06fcd470b0e93a3a84415d6ae06446bbcff8a56603c38d6ed032d792ae915effc921f83a4608fc929'
	x'b2b49e3fa9e8fba262202ec943b2d136851ea4b34f8c9e817568bcf152d50322cfdf64b028d24518388cd0f6303c04d98db6b2572aee28eb5f1a106e887908231984f8801a7d6f8b'
	x'c18e5f5f54142adc415deb00f250aed35532f6d934f4b2f2f590058a9cc7945d2d5a0fdd03debe18b3e3076b57fa1b7b75cbc24df788fef9c06cdb5ff648004a5d75fa6b4543c47f'
	x'973122b49ca3ee2f27a99f0edc4067172ecbfd9ee7b285cd4924c88a596b1fec7289f830df82613b8bc980e4270dfe42276caf623e2f1d38b6888f715a546c685740497ab2e8b697'
	x'abd63c35f39512dea23954528c382a2be7213863b0d6ad9444af495dfc496b30dfe9191eed980d4a3d565e74ad138b684508be0e6cb46293278b4fab3ebae1e5ffa85d3332ff34f9'
	x'8d67244abb2c60b588961bcc53fb2e051d8bc2a0de21415e111b96d9a6aebdf091ad692bd23fe43d1669a6b29cbe597b8779f5c25accdffe7ff9a652de5f4691212c2c492500d5e4'
	x'816b85ad98af064a83b2e342393150e12d22e6072465293f4cbd148dfa31faa4b59904a2a5cc3b12b1aa6a17788bb3e43c4cc5aa791217e0224df4a9d5d0edf8fe0a45809f3b74a0'
	x'b1da18fac27df6182ea92b7e6906432d620942109f83add9ddcdcb1b33323e1ff6ac3ba329d7c088f9b74ccd0a1fb80fe6f7d74d5f06128a12a62dbe5c57f87f543f90832c0ac53d'
	x'03788a68f0bda53ee707abc8582f5cfdb539e3c61c27f90bc74ccc6762e679e8c10a868ab2327b903650921f3e68391c4d5df82be87de234619ec377b94c3408da31c91dbd3b7bf1'
	x'14ba3a3413aa5ea836f6feed5befaf2442bafcc93084ec4914ab5871fe4b6d7b9fbb3c83df3afb54ff36aa6c4794c0de892eac68eee8f4aea3e091550b0cd7f433b5f9f29eda78e5'
	x'75ecdbf6ed279f44199fb7f0ac1b3af577c7761e3f7812481db8e030299a8c8f21449c89ec8ed081f56ad0ac5ef00f8886312e151e0d2deb5630270293f40707baf7bde8274fc6d9'
	x'57103bf0ff2f2d6bd017b349ebc249db'
;


fn void boringssl_borrowed_suite_part0()
{
	test::@check(K_INPUT.len == K_OUTPUT.len, "Input and output lengths don't match.");
	test::@check(K_INPUT.len == K_OVERFLOW_OUTPUT.len, "Input and output lengths don't match.");
}

fn void boringssl_borrowed_suite_ordinary() => @pool()
{
	// cloned mutation
	char[] result = chacha20::tencrypt(K_INPUT, K_KEY, K_NONCE, K_COUNTER);
	test::@check(result == K_OUTPUT, "Failed to get the correct crypt result (cloned).");

	// in-place (mutable) mutation
	char[] mutable = @tclone_slice(K_INPUT); // alt: { ...K_INPUT }
	chacha20::encrypt_mut(mutable, K_KEY, K_NONCE, K_COUNTER);
	test::@check(mutable == K_OUTPUT, "Failed to get the correct crypt result (mutated).");
}

fn void boringssl_borrowed_suite_counter_overflow() => @pool()
{
	// Run the test with the test vector at all lengths.
	for (usz i = 0; i < K_INPUT.len; ++i)
	{
		// cloned mutation
		char[] result = chacha20::tencrypt(K_INPUT[:i], K_KEY, K_NONCE, K_OVERFLOW_COUNTER);
		test::@check(result[:i] == K_OVERFLOW_OUTPUT[:i], "Failed to get the correct crypt result (cloned - index %d).", i);

		// in-place (mutable) mutation
		char[] mutable = @tclone_slice(K_INPUT[:i]); // alt: { ...K_INPUT }
		chacha20::encrypt_mut(mutable, K_KEY, K_NONCE, K_OVERFLOW_COUNTER);
		test::@check(mutable == K_OVERFLOW_OUTPUT[:i], "Failed to get the correct crypt result (mutated - index %d).", i);
	}
}


const char[*] LARGE_INPUT = x'070e151c232a31383f464d545b626970777e858c939aa1a8afb6bdc4cbd2d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb020910171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e555c636a71787f868d949ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d747b828990979ea5acb3bac1c8cfd6dde4ebf2';
const char[*] LARGE_EXPECTED = x'692895b8e56b3f68bf849c9dfb68d845f64936a2512d3a117afe87d505d47de914e83043c0813942d050a8e3de56a63fd229b5108334c5e062d00ee9996e6e4948e4d1e03a7257873d2acacdfafdd1f466000b79092a290d37c12acf6859a0ba330c77936c1a82ff361d1d5915c1af4aeb38814a3f52dfe6e06bf4e5937d5c1162501ca67d07103137ab991b1605584f96cf5bc61d4e97be2ae4ab0a74d8367cfbc3eda6cea389d34dc5758c4ac321a1a8915e234c0b9500dca249e4941beb5bc262f34ab2df9511269c96024191454757fc519269df65bfb4d3af325d8a25d99e38b227addc98cb29753925cecc4956060c95ea2cf038069d543fbb392f';

fn void scrolling_input()
{
	char[*] key = sha256::hash("dance with me");
	char[*] nonce = "123456789abc";

	for (usz i = 1; i < LARGE_INPUT.len; i++) @pool()
	{
		test::@check(chacha20::tencrypt(LARGE_INPUT[:i], key, nonce) == LARGE_EXPECTED[:i], "Mismatch (enc) on index %d", i);
		test::@check(chacha20::tdecrypt(LARGE_EXPECTED[:i], key, nonce) == LARGE_INPUT[:i], "Mismatch (dec) on index %d", i);
	};
}

fn void scrolling_input_randomly_unaligned()
{
	Lcg64Random rand;
	random::seed(&rand, 0x1337_83fb_c1ac_1a20);

	char[*] key = sha256::hash("dance with me");
	char[*] nonce = "123456789abc";

	for (usz i = 1; i < LARGE_INPUT.len; i++) @pool()
	{
		char[] unaligned @align(ulong.sizeof) = mem::talloc_array(char, i + ulong.sizeof);
		char[] encrypt_me = unaligned[((rand.next_byte() % (ulong.sizeof - 1)) + 1) : i];
		encrypt_me[..] = LARGE_INPUT[:i];
		test::@check(chacha20::tencrypt(encrypt_me, key, nonce) == LARGE_EXPECTED[:i], "Mismatch (unaligned) on index %d", i);
	};
}

fn void scrolling_input_unaligned_permutations_with_random_chunks()
{
	// Paranoia, honestly... Use a known test vector a couple blocks long, and - no matter the alignment started from - ensure the same result.
	Lcg64Random rand;
	random::seed(&rand, 0x1337_83fb_c1ac_eeee);

	char[*] key = sha256::hash("dance with me");
	char[*] nonce = "123456789abc";
 
	for (usz i = 1; i < ulong.sizeof + 1; i++)
	{
		for (usz j = 1; j < LARGE_INPUT.len; j++)
        {
            for (usz k = 1; k < 128; k++) @pool()
            {
                char[] unaligned @align(ulong.sizeof) = mem::talloc_array(char, 1 + j + ulong.sizeof);
                unaligned[i:j] = LARGE_INPUT[:j];
                test::@check(chacha20::tencrypt(unaligned[i:j], key, nonce) == LARGE_EXPECTED[:j], "Mismatched permutation of hash on index (%d, %d).", i, j);

                char[] encrypt_me = unaligned[i:j];
                ChaCha20 c @noinit;
                defer c.destroy();
                c.init(key, nonce);
                for (usz x = 1; encrypt_me.len; encrypt_me = encrypt_me[x..], x = (rand.next_byte() % min(k, encrypt_me.len ?: 1)) ?: 1) c.transform(encrypt_me[:x]);
                test::@check(unaligned[i:j] == LARGE_EXPECTED[:j], "Mismatched permutation of hash on index (%d, %d; %d).", i, j, k);

                test::@check(chacha20::tencrypt(LARGE_INPUT[:j], key, nonce) == LARGE_EXPECTED[:j], "Mismatched permutation of hash on index (%d, %d).", i, j);
            };
        }
	}
}


fn void chacha20_chunked_transform() => @pool()
{
	char[*] key = "12345678901234567890123456789012";
	char[*] nonce = "abcdefghijkl";

	char[] original_text = "This is a test string that is longer than a single block.";
	char[] encrypted_once = chacha20::tencrypt(original_text, key, nonce);

	char[] text_to_transform = @tclone_slice(encrypted_once);

	ChaCha20 c @noinit;
	defer c.destroy();
	c.init(key, nonce);

	c.transform(text_to_transform[:3]);
	c.transform(text_to_transform[3..]);

	test::@check(original_text == text_to_transform, "Chunked decryption did not produce the original plaintext.");
}

fn void chacha20_chunked_unaligned_keystream() => @pool()
{
	char[*] key = "12345678901234567890123456789012";
	char[*] nonce = "abcdefghijkl";

	char[] original_text = "This is a test string that is longer than a single block and will be used for this test.";
	char[] encrypted_once = chacha20::tencrypt(original_text, key, nonce);

	char[] unaligned_buffer_storage @align(ulong.sizeof) = mem::talloc_array(char, original_text.len + 1);
	char[] text_to_transform = unaligned_buffer_storage[1..];
	text_to_transform[..] = original_text[..];

	ChaCha20 c @noinit;
	defer c.destroy();
	c.init(key, nonce);

	c.transform(text_to_transform[:3]);
	c.transform(text_to_transform[3..]);

	test::@check(text_to_transform == encrypted_once, "Chunked encryption with unaligned keystream failed.");
}
