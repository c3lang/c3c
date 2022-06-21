
enum EnumWithErrorWithMissingName : int (int) // #error: The parameter must be named
{
    TEST
}

enum EnumWithErrorData : int (int // #error: end of the parameter list
{
    TEST
}

enum EnumWithErrorData2 : int (int, int bar) // #error: The parameter must be named
{
    TEST
}


enum EnumTestErrorType3 : int
{
    A,
    A // #error: This enum constant is declared twice
}