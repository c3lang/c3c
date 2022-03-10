define Number = int;

fn void test1()
{
    10 = 20;        // #error: An assignable expression
}

fn void test2()
{
    "foo" = "bar";  // #error: An assignable expression
}

fn void test3()
{
    true = false;   // #error: An assignable expression
}

fn void test4()
{
    'c' = 'd';      // #error: An assignable expression
}

fn void test5()
{
    3.14 = 2.14;    // #error: An assignable expression
}

fn void test21()
{
    int a = 0;
    int b = 2;
    a++ = b++;      // #error: An assignable expression
}

fn void test22()
{
    var $Type = void;
    $Type = int;
}