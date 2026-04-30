// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::cinterop;
import std::core::env;


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

alias CChar = $typefrom($$C_CHAR_IS_SIGNED ? ichar : char);

enum CBool : char
{
	FALSE,
	TRUE
}

// Helper macros
macro typeid signed_int_from_bitsize(sz $bitsize) @private
{
	$switch $bitsize:
		$case 128: return int128;
		$case 64: return long;
		$case 32: return int;
		$case 16: return short;
		$case 8: return ichar;
		$default: $error("Invalid bitsize");
	$endswitch
}

macro typeid unsigned_int_from_bitsize(sz $bitsize) @private
{
	$switch $bitsize:
		$case 128: return uint128;
		$case 64: return ulong;
		$case 32: return uint;
		$case 16: return ushort;
		$case 8: return char;
		$default: $error("Invalid bitsize");
	$endswitch
}

const USE_STACK_VALIST = env::ARCH_32_BIT || env::WIN32 || (env::DARWIN && env::AARCH64);
module std::core::cinterop @if(USE_STACK_VALIST);

typedef CVaList = void*;
macro CVaList.next(&self, $Type)
{
	void *ptr = mem::aligned_pointer((void*)*self, max($Type::alignment, 8));
	defer *self = (CVaList)(ptr + 1);
	return *($Type*)ptr;
}

module std::core::cinterop @if(env::X86_64 && !env::WIN32);

struct CVaListData
{
    uint gp_offset;
    uint fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
}

typedef CVaList = CVaListData*;

macro CVaList.next(self, $Type)
{
	CVaListData* data = (CVaListData*)self;
	$switch:
	    $case $Type::kind == FLOAT ||| ($Type::kind == VECTOR && $Type::size <= 16):
	        var $LoadType = $Type::size < 8 ? double : $Type;
	        if (data.fp_offset < 6 * 8 + 8 * 16 )
			{
				defer data.fp_offset += (uint)mem::aligned_offset($Type::size, 16);
				return ($Type)*($LoadType*)(data.reg_save_area + data.fp_offset);
			}
			void* ptr = mem::aligned_pointer(data.overflow_arg_area, max(8, $Type::alignment));
			defer data.overflow_arg_area = ptr + $Type::size;
			return ($Type)*($LoadType*)ptr;
		$case $Type::kind == SIGNED_INT || $Type::kind == UNSIGNED_INT:
	        var $LoadType = $Type::size < 4 ? int : $Type;
			if (data.gp_offset < 6 * 8 && $Type::size <= 8)
			{
				defer data.gp_offset += (uint)mem::aligned_offset($Type::size, 8);
				return ($Type)*($LoadType*)(data.reg_save_area + data.gp_offset);
			}
			void* ptr = mem::aligned_pointer(data.overflow_arg_area, max(8, $Type::alignment));
			defer data.overflow_arg_area = ptr + $Type::size;
			return ($Type)*($LoadType*)ptr;
		$default:
			void* ptr = mem::aligned_pointer(data.overflow_arg_area, max(8, $Type::alignment));
			defer data.overflow_arg_area = ptr + $Type::size;
			return *($Type*)ptr;
	$endswitch
}