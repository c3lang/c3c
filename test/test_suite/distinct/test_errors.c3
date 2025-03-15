module test;

typedef Int2 = int;

fn void test()
{
    Int2 a = 1;
    a = a + 1;
    int b;
    a = b; // #error: 'Int2'
}