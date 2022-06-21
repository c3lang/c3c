fn void test()
{
    char* c = { 1, 3, "hello"}[2];
    int x;
    int z = { 1, 3 }[x]; // #error: To subscript a compile time list a compile time integer index is needed
}

fn void test2()
{
    int z = { 1, 3 }[-1]; // #error: The index may not be negative
}

fn void test3()
{
    int z = { 1, 3 }[0xFFFF_FFFF_FFFF_FFFFu64]; // #error: The index is out of range.
}

fn void test4()
{
    int z = { 1, 3 }[2]; // #error: An index of '2' is out of range, a value between 0 and 1 was expected.
    int z2 = { 1 }[2]; // #error: An index of '2' is out of range, a value of 0 was expected.
}

fn void test5()
{
    int z = { 1, 3 }[^4]; // #error: An index of '4' from the end is out of range, a value between 1 and 2 was expected
    int z2 = { 1 }[^4]; // #error: An index of '4' from the end is out of range, a value of 1 was expected.
}
