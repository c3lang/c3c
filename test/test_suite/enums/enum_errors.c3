

enum EnumTestOverflow
{
    VALUE = 0x80000000, // #error: does not fit into 'int'
}

enum EnumTestErrorType : float // #error: must be an integer type not 'float'
{
    VALUE_BOOM
}