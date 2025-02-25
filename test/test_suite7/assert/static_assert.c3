
const int FOO = 2;

fn void test()
{
    $assert FOO == 2 : "Bad";
    $assert FOO == 0 : "Good"; // #error: Good
}