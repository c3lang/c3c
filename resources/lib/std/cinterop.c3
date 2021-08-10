module std::cinterop;

const C_INT_SIZE = ${C_INT_SIZE};
const C_LONG_SIZE = ${C_LONG_SIZE};
const C_SHORT_SIZE = ${C_SHORT_SIZE};
const C_LONG_LONG_SIZE = ${C_LONG_LONG_SIZE};

$assert (C_SHORT_SIZE < 32);
$assert (C_INT_SIZE < 128);
$assert (C_LONG_SIZE < 128);
$assert (C_LONG_LONG_SIZE <= 128);
$assert (C_SHORT_SIZE <= C_INT_SIZE);
$assert (C_INT_SIZE <= C_LONG_SIZE);
$assert (C_LONG_SIZE <= C_LONG_LONG_SIZE);

$if (C_INT_SIZE == 64):
    define CInt = long;
    define CUInt = ulong;
$elif (C_INT_SIZE == 32):
    define CInt = int;
    define CUInt = uint;
$elif (C_INT_SIZE == 16):
    define CInt = short;
    define CUInt = ushort;
$else:
    $assert(false, "Invalid C int size");
$endif;

$if (C_LONG_SIZE == 64):
    define CLong = long;
    define CULong = ulong;
$elif (C_LONG_SIZE == 32):
    define CLong = int;
    define CULong = uint;
$elif (C_LONG_SIZE == 16):
    define CLong = short;
    define CULong = ushort;
$else:
    $assert(false, "Invalid C long size");
$endif;

$if (C_SHORT_SIZE == 32):
    define CShort = int;
    define CUShort = uint;
$elif (C_SHORT_SIZE == 16):
    define CShort = short;
    define CUShort = ushort;
$elif (C_SHORT_SIZE == 8):
    define CShort = ichar;
    define CUShort = char;
$else:
    $assert(false, "Invalid C short size");
$endif;

$if (C_LONG_LONG_SIZE == 128):
    define CLongLong = int128;
    define CULongLong = uint128;
$elif (C_LONG_LONG_SIZE == 64):
    define CLongLong = long;
    define CULongLong = ulong;
$elif (C_LONG_LONG_SIZE == 32):
    define CLongLong = int;
    define CULongLong = uint;
$elif (C_LONG_LONG_SIZE == 16):
    define CLongLong = short;
    define CULongLong = ushort;
$else:
    $assert(false, "Invalid C long long size");
$endif;

define CSChar = ichar;
define CUChar = char;

$if (${C_CHAR_IS_SIGNED}):
    define CChar = ichar;
$else:
    define CChar = char;
$endif;
