typedef char as Number8;
typedef int as Number32;

func void test1()
{
    int a = cast(10 as char);
    int b = cast(200 as char);
    int c = cast(200 as int);
    char  d = cast(200 as int);     // #error: Cannot implicitly cast 'int' to 'char'.
}

func void test2()
{
    char  e = cast(200 as Number32);   // #error: Cannot implicitly cast 'Number32' (int) to 'char'.
}