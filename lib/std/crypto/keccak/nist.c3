// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//   Document Section 2.3.1.
//
module std::crypto::keccak::nist @private;

import std::math, std::bits;
import std::crypto @public;

<* A set of available function names to use when customizing cSHAKE contexts. These are used independently in various Keccak derivatives. *>
enum NistFunctionName : int (String customizer)
{
	NONE {""},
	KMAC {"KMAC"},
	TUPLE_HASH {"TupleHash"},
	PARALLEL_HASH {"ParallelHash"},
}

<* The direction to use when encoding. Determines the placement of the 'size' byte in the resultant array. *>
enum NistEncodingType
{
	LEFT,
	RIGHT,
}

<* A static left-encoded zero-length value. *>
const char[] ZERO_LEFT = { 0x01, 0x00 };
<* A static right-encoded zero-length value. *>
const char[] ZERO_RIGHT = { 0x00, 0x01 };

<*
 Return a compile-time byte array for the given Keccak security level. Whenever this is invoked from a Keccak-based
 construct, it only ever demands LEFT-based encoding (i.e., `left_encode`) of the BYTE size from the security level.

 @require @in($security_level, ...shake::USABLE_LEVELS) : "You must use a valid SHAKE security level."

 @return "A const representation of the input security level as a left-encoded byte sequence."
*>
macro char[] encoded_rate_bytes($security_level) @const
{
	$switch $security_level:
		$case 128: return { 0x01, 0xa8 }; // keccak rate 1344 / 8
		$case 256: return { 0x01, 0x88 }; // keccak rate 1088 / 8
		$default: unreachable("invalid SHAKE security level");
	$endswitch
}

<*
 Encode the given length value as a NIST-formatted sequence of bytes. Section 2.3.1 of the NIST SP 800-185 specifies
 this method and process as part of many Keccak-derived constructs. This function stores the encoded length value in
 a passed slice.

 @param [out] into : "The destination slice in which to store the encoded length."
 @param direction : "Which direction the encoding is intended to be read from."
 @param length : "The length value to encode."
 @param as_direct_bytes : "If set to `true`, encode `length` directly, without converting the value to a bit count."

 @require into.len >= uint128.sizeof + 2 : "The storage buffer must have adequate space for all possible 16-byte integer values (>= 18 bytes)."

 @return "The width of the encoded length in bytes."
*>
fn usz encode_length_into(char[] into, NistEncodingType direction, uint128 length, bool as_direct_bytes = false)
{
	if (!as_direct_bytes) length <<= 3;
	int bits = 8;
	for (; math::pow(2, bits) <= length; bits += 8);
	char bytes = (char)bits / 8;
	uint128 swapped_length = !env::BIG_ENDIAN ??? bswap(length) : length;
	into[1:bytes] = @as_char_view(swapped_length)[^bytes..];
	switch (direction)
	{
		case LEFT:
			into[0] = bytes;
		case RIGHT:
			into[1 + bytes] = bytes;
	}
	return bytes;
}

<*
 Create a locally-scoped byte array and store the length-encoded value of the given length into it. The returned slice
 is always available to the calling scope because this is a macro.

 @param direction : "Which direction the encoding is intended to be read from."
 @param length : "The length value to encode."
 @param as_direct_bytes : "If set to `true`, encode `length` directly, without converting the value to a bit count."

 @return "A slice pointing to a stack-allocated byte array, containing the fully encoded length."
*>
macro char[] encode_length(NistEncodingType direction, uint128 length, bool as_direct_bytes = false)
{
	char[uint128.sizeof + 2] result;
	usz bytes = encode_length_into(result[..], direction, length, as_direct_bytes: as_direct_bytes);
	return direction == LEFT ? result[:bytes + 1] : result[1:bytes + 1];
}
