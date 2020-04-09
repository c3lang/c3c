
enum EnumWithErrorWithMissingName : int (int)
// @error The function parameter must be named
{
    TEST
}

enum EnumWithErrorData : int (int
// @error Unexpected end of parameter list
{
    TEST
}

enum EnumTestOverflow
{
    VALUE = 0x80000000,
// @error does not fit into 'int'
}

enum EnumTestErrorType : float
// @error The enum type must be an integer type not 'float'
{
    VALUE_BOOM
}