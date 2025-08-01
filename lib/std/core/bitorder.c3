// Copyright (c) 2023-2025 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::bitorder;

// This module contains types of different endianness.
// *BE types represent big-endian types
// *LE types represent little-endian types.

bitstruct ShortBE : short @bigendian
{
	short val : 0..15;
}

bitstruct UShortBE : ushort @bigendian
{
	ushort val : 0..15;
}

bitstruct IntBE : int @bigendian
{
	int val : 0..31;
}

bitstruct UIntBE : int @bigendian
{
	uint val : 0..31;
}

bitstruct LongBE : long @bigendian
{
	long val : 0..63;
}

bitstruct ULongBE : ulong @bigendian
{
	ulong val : 0..63;
}

bitstruct Int128BE : int128 @bigendian
{
	int128 val : 0..127;
}

bitstruct UInt128BE : uint128 @bigendian
{
	uint128 val : 0..127;
}

bitstruct ShortLE : short @littleendian
{
	short val : 0..15;
}

bitstruct UShortLE : ushort @littleendian
{
	ushort val : 0..15;
}

bitstruct IntLE : int @littleendian
{
	int val : 0..31;
}

bitstruct UIntLE : int @littleendian
{
	uint val : 0..31;
}

bitstruct LongLE : long @littleendian
{
	long val : 0..63;
}

bitstruct ULongLE : ulong @littleendian
{
	ulong val : 0..63;
}

bitstruct Int128LE : int128 @littleendian
{
	int128 val : 0..127;
}

bitstruct UInt128LE : uint128 @littleendian
{
	uint128 val : 0..127;
}

<*
 @require @is_array_or_slice_of_char(bytes) : "argument must be an array, a pointer to an array or a slice of char"
 @require is_bitorder($Type) : "type must be a bitorder integer"
*>
macro read(bytes, $Type)
{
	char[] s;
	$switch @typekind(bytes):
		$case POINTER:
			s = (*bytes)[:$Type.sizeof];
		$default:
			s = bytes[:$Type.sizeof];
	$endswitch
	return bitcast(*(char[$Type.sizeof]*)s.ptr, $Type).val;
}

<*
 @require @is_arrayptr_or_slice_of_char(bytes) : "argument must be a pointer to an array or a slice of char"
 @require is_bitorder($Type) : "type must be a bitorder integer"
*>
macro write(x, bytes, $Type)
{
	char[] s;
	$switch @typekind(bytes):
		$case POINTER:
			s = (*bytes)[:$Type.sizeof];
		$default:
			s = bytes[:$Type.sizeof];
	$endswitch
	*($typeof(x)*)s.ptr = bitcast(x, $Type).val;
}

macro is_bitorder($Type)
{
	$switch $Type:
		$case UShortLE:
		$case ShortLE:
		$case UIntLE:
		$case IntLE:
		$case ULongLE:
		$case LongLE:
		$case UInt128LE:
		$case Int128LE:
		$case UShortBE:
		$case ShortBE:
		$case UIntBE:
		$case IntBE:
		$case ULongBE:
		$case LongBE:
		$case UInt128BE:
		$case Int128BE:
			return true;
		$default:
			return false;
	$endswitch
}

macro bool is_array_or_slice_of_char(bytes) @deprecated("Use @is_array_or_slice_of_char")
{
	return @is_array_or_slice_of_char(bytes);
}

macro bool @is_array_or_slice_of_char(#bytes) @const
{
    var $Type = $typeof(#bytes);
	$switch $Type.kindof:
		$case POINTER:
			typeid $inner = $Type.inner;
			return $inner.kindof == ARRAY &&& $inner.inner == char.typeid;
		$case ARRAY:
		$case SLICE:
			return $Type.inner == char.typeid;
		$default:
			return false;
	$endswitch
}

macro bool is_arrayptr_or_slice_of_char(bytes) @deprecated("Use @is_arrayptr_or_slice_of_char")
{
	return @is_arrayptr_or_slice_of_char(bytes);
}

macro bool @is_arrayptr_or_slice_of_char(#bytes) @const
{
	var $Type = $typeof(#bytes);
	$switch $Type.kindof:
		$case POINTER:
			typeid $inner = $Type.inner;
			return $inner.kindof == ARRAY &&& $inner.inner == char.typeid;
		$case SLICE:
			return $Type.inner == char.typeid;
		$default:
			return false;
	$endswitch
}