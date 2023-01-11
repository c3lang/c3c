fn void test() @naked
{
    int i; // #error: Only asm statements are allowed inside of a naked function
}