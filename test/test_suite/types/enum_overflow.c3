enum EnumTestOverflow
{
    VALUE = 0x80000000, // #error: does not fit in type 'int'
}