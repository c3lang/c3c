// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

/**
 * @require Enum.kindof == TypeKind.ENUM : "Only enums maybe be used with an enumset"
 **/
module std::collections::enumset<Enum>;

def EnumSetType = $typefrom(private::type_for_enum_elements(Enum.elements)) @private ;

const IS_CHAR_ARRAY = Enum.elements > 128;
def EnumSet = distinct EnumSetType;

fn void EnumSet.add(EnumSet* this, Enum v)
{
	$if IS_CHAR_ARRAY:
		(*this)[(usz)v / 8] |= (char)(1u << ((usz)v % 8));
	$else
        *this = (EnumSet)((EnumSetType)*this | 1u << (EnumSetType)v);
	$endif
}

fn void EnumSet.clear(EnumSet* this)
{
	$if IS_CHAR_ARRAY:
		*this = {};
	$else
        *this = 0;
	$endif
}

fn bool EnumSet.remove(EnumSet* this, Enum v)
{
	$if IS_CHAR_ARRAY:
		if (!this.has(v) @inline) return false;
		(*this)[(usz)v / 8] &= (char)~(1u << ((usz)v % 8));
		return true;
	$else
        EnumSetType old = (EnumSetType)*this;
        EnumSetType new = old & ~(1u << (EnumSetType)v);
        *this = (EnumSet)new;
        return old != new;
	$endif
}

fn bool EnumSet.has(EnumSet* this, Enum v)
{
	$if IS_CHAR_ARRAY:
		return (bool)(((*this)[(usz)v / 8] << ((usz)v % 8)) & 0x01);
	$else
        return ((EnumSetType)*this & (1u << (EnumSetType)v)) != 0;
	$endif
}

fn void EnumSet.add_all(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		foreach (i, c : s) (*this)[i] |= c;
	$else
        *this = (EnumSet)((EnumSetType)*this | (EnumSetType)s);
	$endif
}

fn void EnumSet.retain_all(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		foreach (i, c : s) (*this)[i] &= c;
	$else
        *this = (EnumSet)((EnumSetType)*this & (EnumSetType)s);
	$endif
}

fn void EnumSet.remove_all(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		foreach (i, c : s) (*this)[i] &= ~c;
	$else
        *this = (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
	$endif
}

fn EnumSet EnumSet.and_of(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		EnumSet copy = *this;
		copy.retain_all(s);
		return copy;
	$else
        return (EnumSet)((EnumSetType)*this & (EnumSetType)s);
	$endif
}

fn EnumSet EnumSet.or_of(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		EnumSet copy = *this;
		copy.add_all(s);
		return copy;
	$else
        return (EnumSet)((EnumSetType)*this | (EnumSetType)s);
	$endif
}


fn EnumSet EnumSet.diff_of(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		EnumSet copy = *this;
		copy.remove_all(s);
		return copy;
	$else
        return (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
	$endif
}

fn EnumSet EnumSet.xor_of(EnumSet* this, EnumSet s)
{
	$if IS_CHAR_ARRAY:
		EnumSet copy = *this;
		foreach (i, c : s) copy[i] ^= c;
		return copy;
	$else
        return (EnumSet)((EnumSetType)*this ^ (EnumSetType)s);
	$endif
}

fn void! EnumSet.to_format(EnumSet* set, Formatter* formatter) @dynamic
{
	formatter.print("[")!;
	bool found = false;
	foreach (value : Enum.values)
	{
		if (!set.has(value)) continue;
		if (found) formatter.print(", ")!;
		found = true;
		formatter.printf("%s", value)!;
	}
	formatter.print("]")!;
}

fn String EnumSet.to_string(EnumSet* set, Allocator* using = mem::heap()) @dynamic
{
	return string::printf("%s", *set);
}

module std::collections::enumset::private;

macro typeid type_for_enum_elements(usz $elements)
{
	$switch
		$case ($elements > 128):
			return char[($elements + 7) / 8].typeid;
		$case ($elements > 64):
			return uint128.typeid;
		$case ($elements > 32 || $$C_INT_SIZE > 32):
			return ulong.typeid;
		$case ($elements > 16 || $$C_INT_SIZE > 16):
			return uint.typeid;
		$case ($elements > 8 || $$C_INT_SIZE > 8):
			return ushort.typeid;
		$default:
			return char.typeid;
	$endswitch;
}