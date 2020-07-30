
const int FOO = 2;

func void test()
{
    $assert(FOO == 2, "Bad");
    $assert(FOO == 0, "Good"); // #error: Compile time assert - Good
}