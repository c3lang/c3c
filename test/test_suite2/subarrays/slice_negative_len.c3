fn void test()
{
    int[3] x = { 1, 2, 3};
    int[] z = x[2..2];
    z = x[2..1]; // #error: Start index greater than end index.
}

fn void test2()
{
    int[3] x = { 1, 2, 3};
    int[] z = x[^2..^2];
    z = x[^3..];
    z = x[^1..^2]; // #error: Start index greater than end index.
}

fn void test3()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..4]; // #error: Array end index out of bounds, was 4, exceeding array length 3.
}

fn void test4()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..^1];
    z = x[..^-1]; // #error: Negative numbers are not allowed when indexing from the end.
}

fn void test5()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..^4]; // #error: Array index out of bounds, using a negative array index is only allowed with pointers.
}

fn void test6()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[3..]; // #error: Array index out of bounds, was 3, exceeding max array index 2.
}

fn void test7()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[-1..]; // #error: Array index out of bounds, using a negative array index is only allowed with pointers.
}

fn void test8()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[^4..]; // #error: Array index out of bounds, using a negative array index is only allowed with pointers.
}

fn void test9()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[^0..]; // #error: Array index out of bounds, was 3, exceeding max array index 2.
}

fn void test10()
{
    int* x = null;
    x[-10..-3];
    int[] w = x[0..]; // #error: Omitting end index is not allowed for pointers.
    int[] z = x[^2..]; // #error: Indexing from the end is not allowed for pointers.
    int[] y = x[..^2]; // #error: Indexing from the end is not allowed for pointers.
}

fn void test11()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..^0]; // #error: Array end index out of bounds, was 3, exceeding array length 3
}
