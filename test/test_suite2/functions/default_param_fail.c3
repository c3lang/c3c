
int z;

fault MyError
{
    FOO,
}

fn void test(int a = z) {}

fn void test2(int b = MyError.FOO!) {}