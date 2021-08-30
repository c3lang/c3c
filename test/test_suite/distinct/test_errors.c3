module test;

define Int2 = distinct int;

func void test()
{
    Int2 a = 1;
    a = a + 1;
    int b;
    a = b; // #error: 'int' to 'Int2'
}