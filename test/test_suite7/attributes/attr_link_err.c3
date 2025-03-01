fn void test(int x) @link(true, "abc", "cde")
{
}

fn void testc(int x) @link("abc", "cde")
{
}

fn void test2(int x) @link(true) // #error: Expected a constant string here
{
}
fn void test3(int x) @link(true, 2) // #error: Expected a constant string here
{
}

fn void test4(int x) @link("a", 2) // #error: Expected a constant string here
{
}

fn void test5(int x) @link(true, "a", 5, 5) // #error: Expected a constant string here
{
}
