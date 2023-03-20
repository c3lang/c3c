// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::cinterop;

const C_INT_SIZE = $$C_INT_SIZE;
const C_LONG_SIZE = $$C_LONG_SIZE;
const C_SHORT_SIZE = $$C_SHORT_SIZE;
const C_LONG_LONG_SIZE = $$C_LONG_LONG_SIZE;

$assert (C_SHORT_SIZE < 32);
$assert (C_INT_SIZE < 128);
$assert (C_LONG_SIZE < 128);
$assert (C_LONG_LONG_SIZE <= 128);
$assert (C_SHORT_SIZE <= C_INT_SIZE);
$assert (C_INT_SIZE <= C_LONG_SIZE);
$assert (C_LONG_SIZE <= C_LONG_LONG_SIZE);

$switch ($$C_INT_SIZE)
$case 64:
    typedef CInt = long;
    typedef CUInt = ulong;
$case 32:
    typedef CInt = int;
    typedef CUInt = uint;
$case 16:
    typedef CInt = short;
    typedef CUInt = ushort;
$default:
    $assert(false, "Invalid C int size");
$endswitch

$switch ($$C_LONG_SIZE)
$case 64:
    typedef CLong = long;
    typedef CULong = ulong;
$case 32:
    typedef CLong = int;
    typedef CULong = uint;
$case 16:
    typedef CLong = short;
    typedef CULong = ushort;
$default:
    $assert(false, "Invalid C long size");
$endswitch

$switch ($$C_SHORT_SIZE)
$case 32:
    typedef CShort = int;
    typedef CUShort = uint;
$case 16:
    typedef CShort = short;
    typedef CUShort = ushort;
$case 8:
    typedef CShort = ichar;
    typedef CUShort = char;
$default:
    $assert(false, "Invalid C short size");
$endswitch

$switch ($$C_LONG_LONG_SIZE)
$case 128:
    typedef CLongLong = int128;
    typedef CULongLong = uint128;
$case 64:
    typedef CLongLong = long;
    typedef CULongLong = ulong;
$case 32:
    typedef CLongLong = int;
    typedef CULongLong = uint;
$case 16:
    typedef CLongLong = short;
    typedef CULongLong = ushort;
$default:
    $assert(false, "Invalid C long long size");
$endswitch



typedef CSChar = ichar;
typedef CUChar = char;

$if ($$C_CHAR_IS_SIGNED)
    typedef CChar = ichar;
$else
    typedef CChar = char;
$endif
