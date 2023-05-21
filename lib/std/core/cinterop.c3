// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
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

$switch ($$C_INT_SIZE)
$case 64:
    def CInt = long;
    def CUInt = ulong;
$case 32:
    def CInt = int;
    def CUInt = uint;
$case 16:
    def CInt = short;
    def CUInt = ushort;
$default:
    $error "Invalid C int size";
$endswitch

$switch ($$C_LONG_SIZE)
$case 64:
    def CLong = long;
    def CULong = ulong;
$case 32:
    def CLong = int;
    def CULong = uint;
$case 16:
    def CLong = short;
    def CULong = ushort;
$default:
    $error "Invalid C long size";
$endswitch

$switch ($$C_SHORT_SIZE)
$case 32:
    def CShort = int;
    def CUShort = uint;
$case 16:
    def CShort = short;
    def CUShort = ushort;
$case 8:
    def CShort = ichar;
    def CUShort = char;
$default:
    $error "Invalid C short size";
$endswitch

$switch ($$C_LONG_LONG_SIZE)
$case 128:
    def CLongLong = int128;
    def CULongLong = uint128;
$case 64:
    def CLongLong = long;
    def CULongLong = ulong;
$case 32:
    def CLongLong = int;
    def CULongLong = uint;
$case 16:
    def CLongLong = short;
    def CULongLong = ushort;
$default:
    $error "Invalid C long long size";
$endswitch



def CSChar = ichar;
def CUChar = char;

$if $$C_CHAR_IS_SIGNED:
    def CChar = ichar;
$else
    def CChar = char;
$endif
