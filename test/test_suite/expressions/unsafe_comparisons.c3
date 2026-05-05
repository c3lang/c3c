module test;

fn bool test1(uint x)
{
    return x < 0; // #error: Comparing 'unsigned expression < 0' can never be true
}

fn bool test2(uint x)
{
    return x <= 0; // #error: 'unsigned expression <= 0' is the same as expr == 0
}

fn bool test22(uint x)
{
    return x >= 0; // #error: 'unsigned expression >= 0'
}

fn bool test3(uint x)
{
    return x == -1; // #error:  The value '-1' is out of range for 'uint'
}

fn bool test4(uint x)
{
    return 0 > x ; // #error: '0 > unsigned expression' can never be true
}

fn bool test5(uint x)
{
    return 0 >= x; // #error: '0 >= unsigned expression' is the same as 0 == expr
}

fn bool test55(uint x)
{
    return 0 <= x; // #error: '0 <= unsigned expression'
}

fn bool test6(uint x)
{
    return -1 == x; // #error:  The value '-1' is out of range for 'uint'
}

macro obfuscate(x, $foo)
{
    return $foo == x; // #error:  The value '-1' is out of range for 'uint'
}

fn bool test7(uint x)
{
    return obfuscate(x, -1);
}