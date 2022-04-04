
int z;

fault MyError
{
    FOO,
}

fn void test(int a = z) {} // #error: Only constant expressions may be used as default values

fn void test2(int b = MyError.FOO!) {} // #error: Default arguments may not be failable