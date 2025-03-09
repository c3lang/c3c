
int z;

fault FOO;

fn void test(int a = z) {}

fn void test2(int b = FOO?) {}