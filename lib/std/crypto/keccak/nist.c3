// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//   Document Section 2.3.1.
//
module std::crypto::keccak::nist @private;

import std::math, std::bits;
import std::crypto::shake;

enum NistFunctionName : int (String customizer)
{
	NONE = "",
	KMAC = "KMAC",
	TUPLE_HASH = "TupleHash",
	PARALLEL_HASH = "ParallelHash",
}

enum NistEncodingType
{
	LEFT,
	RIGHT,
}

const char[] ZERO_LEFT = { 0x01, 0x00 };
const char[] ZERO_RIGHT = { 0x00, 0x01 };

<*
 Return a compile-time char array for the given Keccak security level. Whenever this is invoked from a Keccak-based
 construct, it only ever demands LEFT-based encoding (i.e., `left_encode`) of the BYTE size from the security level.

 @require @in($security_level, ...shake::USABLE_LEVELS) : "You must use a valid SHAKE security level."
*>
macro char[] encoded_rate_bytes($security_level) @const
{
	$switch $security_level:
		$case 128: return { 0x01, 0xa8 }; // keccak rate 1344 / 8
		$case 256: return { 0x01, 0x88 }; // keccak rate 1088 / 8
		$default: unreachable("invalid SHAKE security level");
	$endswitch
}

macro char[] encode_length(NistEncodingType $direction, uint128 #length, bool $as_direct_bytes = false) @safemacro
{
	char[uint128.sizeof + 2] result;
	uint128 length = $as_direct_bytes ??? #length : #length * 8ull;
	// ensure, if length is converted to bits, that the top 3 bits of the input aren't set
	$if !$as_direct_bytes: assert(length >> 125 == 0); $endif
	int bits = 8;
	for (; math::pow(2, bits) <= length; bits += 8);
	char bytes = (char)bits / 8;
	uint128 swapped_length = !env::BIG_ENDIAN ??? bswap(length) : length;
	char[] sliced_length = @as_char_view(swapped_length);
	result[1:bytes] = sliced_length[^bytes..];
	switch
	{
		case $direction == LEFT:
			result[0] = bytes;
			return result[:bytes + 1];
		case $direction == RIGHT:
			result[1 + bytes] = bytes;
			return result[1:bytes + 1];
	}
	unreachable();
}
