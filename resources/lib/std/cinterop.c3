module std::cinterop;

$assert (${C_SHORT_SIZE} < 32);
$assert (${C_LONG_TYPE) < 128);

$if (${C_INT_SIZE} == 64)
{
    define CInt = long;
    define CUInt = ulong;
}
$elseif (${C_INT_SIZE} == 32)
{
    define CInt = int;
    define CUInt = uint;
}
$elseif (${C_INT_SIZE} == 16)
{
    define CInt = short;
    define CUInt = ushort;
}
$else
{
    $assert(false, "Invalid C int size");
}

$if (${C_LONG_SIZE} == 64)
{
    define CLong = long;
    define CULong = ulong;
}
$elseif (${C_LONG_SIZE} == 32)
{
    define CLong = int;
    define CULong = uint;
}
$elseif (${C_LONG_SIZE} == 16)
{
    define CLong = short;
    define CULong = ushort;
}
$else
{
    $assert(false, "Invalid C long size");
}

$if (${C_SHORT_SIZE} == 32)
{
    define CShort = int;
    define CUShort = uint;
}
$elseif (${C_SHORT_SIZE} == 16)
{
    define CShort = short;
    define CUShort = ushort;
}
$elseif (${C_SHORT_SIZE} == 8)
{
    define CShort = ichar;
    define CUShort = char;
}
$else
{
    $assert(false, "Invalid C short size");
}

/*
define CChar = ${C_CHAR_TYPE};
define CSChar = ${C_SCHAR_TYPE};
define CShort = ${C_SHORT_TYPE};
define CInt = ${C_INT_TYPE};
define CLong = $(C_LONG_TYPE);
define CLongLong = ${C_LONGLONG_TYPE};
define CUChar = ${C_UCHAR_TYPE};
define CUShort = ${C_USHORT_TYPE};
define CUInt = ${C_UINT_TYPE};
define CULong = $(C_ULONG_TYPE);
define CULongLong = ${C_ULONGLONG_TYPE};
*/
