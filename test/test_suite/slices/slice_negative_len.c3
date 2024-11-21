fn void test()
{
    int[3] x = { 1, 2, 3};
    int[] z = x[2..2];
    z = x[2..1]; // #error: greater than the end index
}

fn void test2()
{
    int[3] x = { 1, 2, 3};
    int[] z = x[^2..^2];
    z = x[^3..];
    z = x[^1..^2]; // #error: greater than the end index
}

fn void test3()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..4]; // #error: End index out of bounds, was 4, exceeding
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
    int[] z = x[..^4]; // #error: An index may only be negative
}

fn void test6()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[3..]; // #error: Index out of bounds: the start index was
}

fn void test7()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[-1..]; // #error: An index may only be negative
}

fn void test8()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[^4..]; // #error: An index may only be negative
}

fn void test9()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[^0..]; // #error: Index out of bounds: the start
}

fn void test10()
{
    int* x = null;
    x[-10..-3];
    int[] w = x[0..]; // #error: Omitting end index is not allowed for pointers
    int[] z = x[^2..]; // #error: Indexing from the end is not allowed for pointers
    int[] y = x[..^2]; // #error: Indexing from the end is not allowed for pointers
}

struct Abc
{
	int a;
	char[*] z;
}

fn void test105()
{
	Abc a;
    int[] w = a.z[0..]; // #error: Omitting end index is not allowed for pointers
    int[] z = a.z[^2..]; // #error: Indexing from the end is not allowed for pointers
    int[] y = a.z[..^2]; // #error: Indexing from the end is not allowed for pointers
    a.z[-10..-3]; // #error: An index may only be negative for pointers
}

fn void test11()
{
    int[3] x = { 1, 2, 3 };
    int[] z = x[..^0]; // #error: End index out of bounds, was 3, exceeding
}
