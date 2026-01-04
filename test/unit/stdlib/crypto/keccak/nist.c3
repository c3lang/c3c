// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module keccak_test::nist @test;

import std::crypto::keccak::nist @public;
import std::crypto::keccak;
import std::bits;


fn void consts()
{
	test::eq(nist::encode_length(LEFT, 0, false), nist::ZERO_LEFT);
	test::eq(nist::encode_length(LEFT, 0, true), nist::ZERO_LEFT);
	test::eq(nist::encode_length(RIGHT, 0, false), nist::ZERO_RIGHT);
	test::eq(nist::encode_length(RIGHT, 0, true), nist::ZERO_RIGHT);
	test::eq(nist::encode_length(LEFT, keccak::rate(128) / 8), nist::encoded_rate_bytes(128));
	test::eq(nist::encode_length(LEFT, keccak::rate(256) / 8), nist::encoded_rate_bytes(256));
}

fn void basic_left_encode()
{
	test::@check(nist::encode_length(LEFT, 0, false) == x'01 00');
	test::@check(nist::encode_length(LEFT, 0, true) == x'01 00');
	test::@check(nist::encode_length(LEFT, 1, false) == x'01 08');
	test::@check(nist::encode_length(LEFT, 1, true) == x'01 01');
	test::@check(nist::encode_length(LEFT, 65538, false) == x'03 080010');
	test::@check(nist::encode_length(LEFT, 65538, true) == x'03 010002');
	test::@check(nist::encode_length(LEFT, 0x212477b7a32ac850ULL, false) == x'09 010923bdbd19564280');
	test::@check(nist::encode_length(LEFT, 0x212477b7a32ac850ULL, true) == x'08 212477b7a32ac850');
}

fn void basic_right_encode()
{
	test::@check(nist::encode_length(RIGHT, 0, false) == x'00 01');
	test::@check(nist::encode_length(RIGHT, 0, true) == x'00 01');
	test::@check(nist::encode_length(RIGHT, 1, false) == x'08 01');
	test::@check(nist::encode_length(RIGHT, 1, true) == x'01 01');
	test::@check(nist::encode_length(RIGHT, 65538, false) == x'080010 03');
	test::@check(nist::encode_length(RIGHT, 65538, true) == x'010002 03');
	test::@check(nist::encode_length(RIGHT, 0x212477b7a32ac850ULL, false) == x'010923bdbd19564280 09');
	test::@check(nist::encode_length(RIGHT, 0x212477b7a32ac850ULL, true) == x'212477b7a32ac850 08');
}

fn void stepped_combinations()
{
	test::@check(nist::encode_length(LEFT, 0x06, false) == x'01 30');
	test::@check(nist::encode_length(LEFT, 0x06, true) == x'01 06');
	test::@check(nist::encode_length(RIGHT, 0x06, false) == x'30 01');
	test::@check(nist::encode_length(RIGHT, 0x06, true) == x'06 01');

	test::@check(nist::encode_length(LEFT, 0x66, false) == x'02 0330');
	test::@check(nist::encode_length(LEFT, 0x66, true) == x'01 66');
	test::@check(nist::encode_length(RIGHT, 0x66, false) == x'0330 02');
	test::@check(nist::encode_length(RIGHT, 0x66, true) == x'66 01');

	test::@check(nist::encode_length(LEFT, 0x0666, false) == x'02 3330');
	test::@check(nist::encode_length(LEFT, 0x0666, true) == x'02 0666');
	test::@check(nist::encode_length(RIGHT, 0x0666, false) == x'3330 02');
	test::@check(nist::encode_length(RIGHT, 0x0666, true) == x'0666 02');

	test::@check(nist::encode_length(LEFT, 0x6666, false) == x'03 033330');
	test::@check(nist::encode_length(LEFT, 0x6666, true) == x'02 6666');
	test::@check(nist::encode_length(RIGHT, 0x6666, false) == x'033330 03');
	test::@check(nist::encode_length(RIGHT, 0x6666, true) == x'6666 02');

	char[uint128.sizeof + 2] byte_result;
	char[uint128.sizeof + 2] bit_result;
	for (char i = 0, uint128 x = 0x06; i < 30; i++, x = (x << 4) | 0x06ull)
	{
		uint128 bits = x << 3;
		char byte_count = (i / 2) + 1;
		byte_result[0] = byte_result[1 + byte_count] = byte_count;
		byte_result[1:byte_count] = @as_char_view(env::BIG_ENDIAN ??? x : *&&bswap(x))[^byte_count..];
		char bit_count = byte_count + (i % 2);
		bit_result[0] = bit_result[1 + bit_count] = bit_count;
		bit_result[1:bit_count] = @as_char_view(env::BIG_ENDIAN ??? bits : *&&bswap(bits))[^bit_count..];
		test::eq(nist::encode_length(LEFT, x, false), bit_result[0 : 1 + bit_count]);
		test::eq(nist::encode_length(LEFT, x, true), byte_result[0 : 1 + byte_count]);
		test::eq(nist::encode_length(RIGHT, x, false), bit_result[1 : 1 + bit_count]);
		test::eq(nist::encode_length(RIGHT, x, true), byte_result[1 : 1 + byte_count]);
	}
}
