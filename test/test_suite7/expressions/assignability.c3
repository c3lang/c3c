def Number = int;

fn void test1()
{
    10 = 20;        // #error: to a constant expression
}

fn void test2()
{
    "foo" = "bar";  // #error: to a constant expression
}

fn void test3()
{
    true = false;   // #error: to a constant expression
}

fn void test4()
{
    'c' = 'd';      // #error: to a constant expression
}

fn void test5()
{
    3.14 = 2.14;    // #error: to a constant expression
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