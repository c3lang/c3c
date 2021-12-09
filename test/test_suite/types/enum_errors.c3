
enum EnumTestErrorType3 : int
{
    A = FOO // #error: 'FOO' could not be found, did you spell it
}

fn int foo()
{
    return 10;
}

enum State
{
    A = foo(),      // #error: Expected a constant expression for enum
    B = "hello",    // #error: 'char[5]*' into 'int'
    C = true,       // #error: 'bool' to 'int'
}
