// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::cinterop;
import std::core::env;


<* The size in bits of the C `int` type on this platform. *>
const C_INT_SIZE = $$C_INT_SIZE;
<* The size in bits of the C `long` type on this platform. *>
const C_LONG_SIZE = $$C_LONG_SIZE;
<* The size in bits of the C `short` type on this platform. *>
const C_SHORT_SIZE = $$C_SHORT_SIZE;
<* The size in bits of the C `long long` type on this platform. *>
const C_LONG_LONG_SIZE = $$C_LONG_LONG_SIZE;

$assert C_SHORT_SIZE < 32;
$assert C_INT_SIZE < 128;
$assert C_LONG_SIZE < 128;
$assert C_LONG_LONG_SIZE <= 128;
$assert C_SHORT_SIZE <= C_INT_SIZE;
$assert C_INT_SIZE <= C_LONG_SIZE;
$assert C_LONG_SIZE <= C_LONG_LONG_SIZE;

<* The C `short` type on this platform. *>
alias CShort = signed_int_from_bitsize($$C_SHORT_SIZE);
<* The C `unsigned short` type on this platform. *>
alias CUShort = unsigned_int_from_bitsize($$C_SHORT_SIZE);
<* The C `int` type on this platform. *>
alias CInt = signed_int_from_bitsize($$C_INT_SIZE);
<* The C `unsigned int` type on this platform. *>
alias CUInt = unsigned_int_from_bitsize($$C_INT_SIZE);
<* The C `long` type on this platform. *>
alias CLong = signed_int_from_bitsize($$C_LONG_SIZE);
<* The C `unsigned long` type on this platform. *>
alias CULong = unsigned_int_from_bitsize($$C_LONG_SIZE);
<* The C `long long` type on this platform. *>
alias CLongLong = signed_int_from_bitsize($$C_LONG_LONG_SIZE);
<* The C `unsigned long long` type on this platform. *>
alias CULongLong = unsigned_int_from_bitsize($$C_LONG_LONG_SIZE);
<* The C `signed char` type. *>
alias CSChar = ichar;
<* The C `unsigned char` type. *>
alias CUChar = char;

<* The C `char` type on this platform: signed or unsigned depending on the target ABI. *>
alias CChar = $$C_CHAR_IS_SIGNED ??? ichar : char;

<* A C-ABI compatible boolean, stored as a single byte. *>
enum CBool : char
{
	FALSE,
	TRUE
}

// Helper macros
<*
 Map a bitsize to the matching signed C3 integer type.

 @param $bitsize : "The size in bits: 8, 16, 32, 64 or 128"
 @return "The signed integer type of that bitsize"

 @require [$bitsize] $bitsize == 8 || $bitsize == 16 || $bitsize == 32 || $bitsize == 64 || $bitsize == 128 : "Invalid bitsize"
*>
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

<*
 Map a bitsize to the matching unsigned C3 integer type.

 @param $bitsize : "The size in bits: 8, 16, 32, 64 or 128"
 @return "The unsigned integer type of that bitsize"

 @require [$bitsize] $bitsize == 8 || $bitsize == 16 || $bitsize == 32 || $bitsize == 64 || $bitsize == 128 : "Invalid bitsize"
*>
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

module std::core::cinterop @feat(ARCH_32_BIT, WIN32, DARWIN & AARCH64);

<*
 A C `va_list`, for platforms where variadic arguments are read from a simple, linearly
 advancing, pointer-aligned buffer (32-bit architectures, Windows, and Darwin/AArch64).
*>
typedef CVaList = void*;

<*
 Fetch the next variadic argument of the given type and advance the list past it.

 @param [&inout] self : "The va_list to read from and advance"
 @param $Type : "The type of the next argument"
 @return "The next argument, read from the va_list"
*>
macro CVaList.next(&self, $Type)
{
	void *ptr = mem::aligned_pointer((void*)*self, max($Type::alignment, 8));
	defer *self = (CVaList)(ptr + $Type.size);
	return *($Type*)ptr;
}

module std::core::cinterop @feat(X86_64) @feat(!WIN32);

<*
 The SysV AMD64 ABI's `va_list` layout: tracks how much of the register save area (both the
 general-purpose and floating-point/SSE register areas) has already been consumed by prior
 `next` calls, plus a pointer into the stack-based overflow area for arguments that don't fit
 in registers.
*>
struct CVaListData
{
    uint gp_offset;
    uint fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
}

<*
 A C `va_list` on the SysV AMD64 ABI (Linux/macOS/etc. on x86-64, excluding Windows).
*>
typedef CVaList = CVaListData*;

<*
 Fetch the next variadic argument of the given type and advance the list past it, following
 the SysV AMD64 ABI's register-save-area / stack-overflow-area rules.

 @param [&inout] self : "The va_list to read from and advance"
 @param $Type : "The type of the next argument"
 @return "The next argument, read from the register save area or the stack overflow area"
*>
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
