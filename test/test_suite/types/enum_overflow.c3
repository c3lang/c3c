enum EnumTestOverflow
{
    VALUE = 0x80000000i64, // #error: Cannot narrow 'long' to 'int'
}