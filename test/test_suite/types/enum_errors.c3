

enum EnumTestOverflow
{
    VALUE = 0x80000000, // #error: does not fit in type 'int'
}

enum EnumTestErrorType : float // #error: must be an integer type not 'float'
{
    VALUE_BOOM
}


enum EnumWithErrorType2 : int* // #error: must be an integer type not 'int*'
{
    TEST
}

enum EnumTestErrorType3 : int
{
    A = FOO // #error: Identifier 'FOO' could not be found
}

func int foo()
{
    return 10;
}

enum State
{
    A = foo(),      // #error: Expected a constant expression for enum
    B = "hello",    // #error: Cannot implicitly cast 'string' to 'int'
    C = true,       // #error: Cannot implicitly cast 'bool' to 'int'
}
