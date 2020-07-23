enum EnumTest : long
{
    VALUE1 = 4,
    VALUE2
}

typedef long as Frob;

enum EnumTestAlias : Frob
{
    VALUE1 = 4,
    VALUE2
}

enum EnumTestDefault
{
    VALUE,
    VALUE2
}

enum EnumTestNoOverflowAfterLong : long
{
    VALUE = 0x7FFF_FFFF_FFFF_FFFE,
    VALUE_NO_EXCEED
}

enum EnumTestSmall : ushort
{
    VALUE = 0xFF,
    VALUE2 = 0xFFFF
}

enum EnumWithErrorData2 : int (int bar, )
{
    TEST
}

enum EnumTestErrorType4
{
}

enum EnumTest5
{
    B = 0,
    C,
}