// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::cinterop;

const C_INT_SIZE = $$C_INT_SIZE;
const C_LONG_SIZE = $$C_LONG_SIZE;
const C_SHORT_SIZE = $$C_SHORT_SIZE;
const C_LONG_LONG_SIZE = $$C_LONG_LONG_SIZE;

$assert C_SHORT_SIZE < 32;
$assert C_INT_SIZE < 128;
$assert C_LONG_SIZE < 128;
$assert C_LONG_LONG_SIZE <= 128;
$assert C_SHORT_SIZE <= C_INT_SIZE;
$assert C_INT_SIZE <= C_LONG_SIZE;
$assert C_LONG_SIZE <= C_LONG_LONG_SIZE;

alias CShort = $typefrom(signed_int_from_bitsize($$C_SHORT_SIZE));
alias CUShort = $typefrom(unsigned_int_from_bitsize($$C_SHORT_SIZE));
alias CInt = $typefrom(signed_int_from_bitsize($$C_INT_SIZE));
alias CUInt = $typefrom(unsigned_int_from_bitsize($$C_INT_SIZE));
alias CLong = $typefrom(signed_int_from_bitsize($$C_LONG_SIZE));
alias CULong = $typefrom(unsigned_int_from_bitsize($$C_LONG_SIZE));
alias CLongLong = $typefrom(signed_int_from_bitsize($$C_LONG_LONG_SIZE));
alias CULongLong = $typefrom(unsigned_int_from_bitsize($$C_LONG_LONG_SIZE));
alias CSChar = ichar;
alias CUChar = char;

alias CChar = $typefrom($$C_CHAR_IS_SIGNED ? ichar.typeid : char.typeid);

enum CBool : char
{
	FALSE,
	TRUE
}

// Helper macros
macro typeid signed_int_from_bitsize(usz $bitsize) @private
{
	$switch $bitsize:
		$case 128: return int128.typeid;
		$case 64: return long.typeid;
		$case 32: return int.typeid;
		$case 16: return short.typeid;
		$case 8: return ichar.typeid;
		$default: $error("Invalid bitsize");
	$endswitch
}

macro typeid unsigned_int_from_bitsize(usz $bitsize) @private
{
	$switch $bitsize:
		$case 128: return uint128.typeid;
		$case 64: return ulong.typeid;
		$case 32: return uint.typeid;
		$case 16: return ushort.typeid;
		$case 8: return char.typeid;
		$default: $error("Invalid bitsize");
	$endswitch
}
