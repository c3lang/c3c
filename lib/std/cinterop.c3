// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::cinterop;

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

$switch ($$C_INT_SIZE):
$case 64:
    define CInt = long;
    define CUInt = ulong;
$case 32:
    define CInt = int;
    define CUInt = uint;
$case 16:
    define CInt = short;
    define CUInt = ushort;
$default:
    $assert(false, "Invalid C int size");
$endswitch;

$switch ($$C_LONG_SIZE):
$case 64:
    define CLong = long;
    define CULong = ulong;
$case 32:
    define CLong = int;
    define CULong = uint;
$case 16:
    define CLong = short;
    define CULong = ushort;
$default:
    $assert(false, "Invalid C long size");
$endswitch;

$switch ($$C_SHORT_SIZE):
$case 32:
    define CShort = int;
    define CUShort = uint;
$case 16:
    define CShort = short;
    define CUShort = ushort;
$case 8:
    define CShort = ichar;
    define CUShort = char;
$default:
    $assert(false, "Invalid C short size");
$endswitch;

$switch ($$C_LONG_LONG_SIZE):
$case 128:
    define CLongLong = int128;
    define CULongLong = uint128;
$case 64:
    define CLongLong = long;
    define CULongLong = ulong;
$case 32:
    define CLongLong = int;
    define CULongLong = uint;
$case 16:
    define CLongLong = short;
    define CULongLong = ushort;
$default:
    $assert(false, "Invalid C long long size");
$endswitch;



define CSChar = ichar;
define CUChar = char;

$if ($$C_CHAR_IS_SIGNED):
    define CChar = ichar;
$else:
    define CChar = char;
$endif;
