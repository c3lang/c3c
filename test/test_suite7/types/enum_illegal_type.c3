enum EnumTestErrorType : float // #error: must be an integer type not 'float'
{
    VALUE_BOOM
}

enum EnumWithErrorType2 : int* // #error: must be an integer type not 'int*'
{
    TEST
}