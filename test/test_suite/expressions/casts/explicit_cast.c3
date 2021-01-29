typedef char as Number8;
typedef int as Number32;

func void test1()
{
    int a = cast(10 as ichar);
    int b = cast(200 as ichar);
    int c = cast(200 as int);
    ichar d = cast(200 as int);     // #error: Cannot implicitly cast 'int' to 'ichar'.
}

func void test2()
{
    char  e = cast(200 as Number32);   // #error: Cannot implicitly cast 'Number32' (int) to 'char'.
}