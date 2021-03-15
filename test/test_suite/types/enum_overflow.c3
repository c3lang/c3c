enum EnumTestOverflow
{
    VALUE = 0x80000000, // #error: The value '2147483648' is out of range for 'int'
}