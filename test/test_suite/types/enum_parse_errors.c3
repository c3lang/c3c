
enum EnumWithErrorWithMissingName : int (int) // #error: Expected a member name here
{
    TEST
}

enum EnumWithErrorData : int (int
{ // #error: Expected a member name here
    TEST
}

enum EnumWithErrorData2 : int (int, int bar) // #error: Expected a member name here
{
    TEST
}


enum EnumTestErrorType3 : int
{
    A,
    A // #error: This enum constant is declared twice
}