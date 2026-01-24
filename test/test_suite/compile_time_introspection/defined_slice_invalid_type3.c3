fn void test1()
{
     bool a = $defined(void[<*>]); // #error: 'void' is not of a vectorizable type
}
