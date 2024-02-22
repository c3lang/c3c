
enum EnumWithErrorWithMissingName : (int)  // #error: Expected a member name here
{
    TEST
}

enum EnumWithErrorData : ( int
{ // #error: Expected a member name here
    TEST
}

enum EnumWithErrorData2 :  ( int, int bar ) // #error: Expected a member name here
{
    TEST
}


enum EnumTestErrorType3 : int
{
    A,
    A // #error: This enum constant is declared twice
}