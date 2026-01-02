// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// See: NIST SP 800-185: https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-185.pdf
//   Document Section 2.3.1.
//
module std::hash::sha3::nist @private;

import std::math;


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


macro char[] @ct_encode_length(NistEncodingType $direction, usz $length) @const
{
	char[usz.sizeof + 1] $buf;

	usz $bit_size = (usz.sizeof * 8) - @clz($length) + 3;
	usz $byte_size = ($bit_size / 8) + ($bit_size % 8 ? 1 : 0);

	usz $len = $byte_size + 1;

	usz $end;
	$if $direction == RIGHT:
		$buf[$byte_size] = (char)$byte_size;
		$end = $byte_size - 1;
	$else
		$end = $byte_size;
	$endif

	$buf[$end] = (char)($length << 3);

	usz $len_sub = $length >> 5;

	$for var $i = 1; $i < $byte_size; ++$i, $len_sub >>= 8:
		$buf[$end - $i] = (char)$len_sub;
	$endfor

	$if $direction == LEFT:
		$buf[0] = (char)$byte_size;
	$endif

	return $buf[:$len];
}


macro char[] encode_length(NistEncodingType $direction, usz length, bool $as_direct_bytes = false)
{
	char[usz.sizeof + 1] buf;

	usz bit_size = (usz.sizeof * 8) - length.clz() + 3;
	usz byte_size = (usz)math::ceil(bit_size / 8.0);

	usz len = byte_size + 1;

	usz end;
	$if $direction == RIGHT:
		buf[byte_size] = (char)byte_size;
		end = byte_size - 1;
	$else
		end = byte_size;
	$endif

	$if $as_direct_bytes:
		buf[end] = (char)length;
		usz len_sub = length >> 8;
	$else
		buf[end] = (char)(length << 3);
		usz len_sub = length >> 5;
	$endif

	for (usz i = 1; i < byte_size; ++i, len_sub >>= 8) buf[end - i] = (char)len_sub;

	$if $direction == LEFT:
		buf[0] = (char)byte_size;
	$endif

	return buf[:len];
}
