
enum EnumTestErrorType3 : int
{
    A = FOO // #error: 'FOO' could not be found, did you spell it
}

func int foo()
{
    return 10;
}

enum State
{
    A = foo(),      // #error: Expected a constant expression for enum
    B = "hello",    // #error: 'string literal' into 'int'
    C = true,       // #error: 'bool' to 'int'
}
