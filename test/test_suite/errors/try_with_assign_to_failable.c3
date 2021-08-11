func void test()
{
    int! z;
    int! w;
    try w = z; // #error: A 'try' assignment is not possible with failable on the left hand side, did you intend 'try (variable = expr)
}

func int! err();
func void test2()
{
    int! z;
    int! w;
    try err() = z; // #error: This expression is not assignable, did you make a mistake
}