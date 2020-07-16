
enum EnumWithErrorWithMissingName : int (int) // #error: function parameter must be named
{
    TEST
}

enum EnumWithErrorData : int (int // #error: end of the parameter list
{
    TEST
}

error TheError
{
    union // #error: A type name was expected here
    {
        int a;
        int b;
    }
}