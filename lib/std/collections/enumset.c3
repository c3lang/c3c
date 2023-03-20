// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

/**
 * @require Enum.kindof == TypeKind.ENUM : "Only enums maybe be used with an enumset"
 **/
module std::collections::enumset<Enum>;


$if (Enum.elements > 128)
typedef EnumSetType @private = char[(Enum.elements + 7) / 8];
const IS_CHAR_ARRAY = true;
$elif (Enum.elements > 64)
typedef EnumSetType @private = uint128;
const IS_CHAR_ARRAY = false;
$elif (Enum.elements > 32 || $$C_INT_SIZE > 32)
typedef EnumSetType @private = ulong;
const IS_CHAR_ARRAY = false;
$elif (Enum.elements > 16 || $$C_INT_SIZE > 16)
typedef EnumSetType @private = uint;
const IS_CHAR_ARRAY = false;
$elif (Enum.elements > 8 || $$C_INT_SIZE > 8)
typedef EnumSetType @private = ushort;
const IS_CHAR_ARRAY = false;
$else
typedef EnumSetType @private = char;
const IS_CHAR_ARRAY = false;
$endif

typedef EnumSet = distinct EnumSetType;

fn void EnumSet.add(EnumSet* this, Enum v)
{
$if (IS_CHAR_ARRAY)
	(*this)[v / 8] |= (char)(1u << (v % 8));
$else
    *this = (EnumSet)((EnumSetType)*this | 1u << (EnumSetType)v);
$endif
}

fn void EnumSet.clear(EnumSet* this)
{
$if (IS_CHAR_ARRAY)
	*this = {};
$else
    *this = 0;
$endif
}

fn bool EnumSet.remove(EnumSet* this, Enum v)
{
$if (IS_CHAR_ARRAY)
	if (!this.has(v) @inline) return false;
	(*this)[v / 8] &= (char)~(1 << (v % 8));
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
$if (IS_CHAR_ARRAY)
	return (bool)(((*this)[v / 8] << (v % 8)) & 0x01);
$else
    return ((EnumSetType)*this & (1u << (EnumSetType)v)) != 0;
$endif
}

fn void EnumSet.add_all(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	foreach (i, c : s) (*this)[i] |= c;
$else
    *this = (EnumSet)((EnumSetType)*this | (EnumSetType)s);
$endif
}

fn void EnumSet.retain_all(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	foreach (i, c : s) (*this)[i] &= c;
$else
    *this = (EnumSet)((EnumSetType)*this & (EnumSetType)s);
$endif
}

fn void EnumSet.remove_all(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	foreach (i, c : s) (*this)[i] &= ~c;
$else
    *this = (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
$endif
}

fn EnumSet EnumSet.and_of(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	EnumSet copy = *this;
	copy.retain_all(s);
	return copy;
$else
    return (EnumSet)((EnumSetType)*this & (EnumSetType)s);
$endif
}

fn EnumSet EnumSet.or_of(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	EnumSet copy = *this;
	copy.add_all(s);
	return copy;
$else
    return (EnumSet)((EnumSetType)*this | (EnumSetType)s);
$endif
}


fn EnumSet EnumSet.diff_of(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	EnumSet copy = *this;
	copy.remove_all(s);
	return copy;
$else
    return (EnumSet)((EnumSetType)*this & ~(EnumSetType)s);
$endif
}

fn EnumSet EnumSet.xor_of(EnumSet* this, EnumSet s)
{
$if (IS_CHAR_ARRAY)
	EnumSet copy = *this;
	foreach (i, c : s) copy[i] ^= c;
	return copy;
$else
    return (EnumSet)((EnumSetType)*this ^ (EnumSetType)s);
$endif
}