
enum Foo
{
    A, B
}

enum Bar
{
    B
}

fn void test_other_enum()
{
    Foo f = A;
    switch (f)
    {
        case Foo.A:
            break;
        case B:
            break;
        case Bar.B: // #error: to 'Foo'
            break;
    }
}

fn void test_check_nums()
{
    Foo f = A;
    switch (f)
    {
        case Foo.from_ordinal(1):
            break;
        case Foo.from_ordinal(0):
            break;
    }
}

fn void test_scope(int i)
{
    switch (i)
    {
        case 1:
            int a = 0;
            break;
        case 2:
            test_scope(a + 1); // #error: 'a' could not be found, did you spell it right
    }
}

fn void test_duplicate_case(int i)
{
    switch (i)
    {
        case 1:
            break;
        case 2:
            break;
        case 1:     // #error: same case value appears
            break;
    }
}

fn void test_duplicate_case2(Foo i)
{
    switch (i)
    {
        case A:
            break;
        case Foo.from_ordinal(1):
            break;
        case A:     // #error: same case value appears
            break;
    }
}

fn void test_duplicate_case3(Foo i)
{
    switch (i)
    {
        case A:
            break;
        case Foo.from_ordinal(0):     // #error: same case value appears
            break;
    }
}

enum Baz
{
    A, B, C, D
}

fn void test_missing_all_cases(Baz x)
{
    switch (x) // #error: not handled in the switch: A, B, C, ...
    {
    }
}

fn void test_missing_some_cases(Baz x)
{
    switch (x) // #error: not handled in the switch: B, C and D
    {
        case A:
            break;
    }
}

fn void test_missing_some_cases2(Baz x)
{
    switch (x) // #error: B and D
    {
        case C:
        case A:
            break;
    }
}

fn void test_missing_some_cases3(Baz x)
{
    switch (x) // #error: Enum value D was not handled in the switch
    {
        case B:
        case C:
        case A:
            break;
    }
}

fn void test_missing_no_cases(Baz x)
{
    switch (x)
    {
        default:
            break;
    }
}

fault MathError
{
    DIVISION_BY_ZERO
}

// Rethrowing an error uses "!!" suffix
fn void! testMayError()
{
}

fn void main()
{
    // Handle the error
    switch (catch err = testMayError()) // #error: Catch unwrapping is only allowed
    {
        case MathError.DIVISION_BY_ZERO:
            libc::printf("Division by zero\n");
            return;
        default:
            libc::printf("Unexpected error!");
            return;
    }
}