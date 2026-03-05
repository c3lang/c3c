
int z;

faultdef FOO;

fn void test(int a = z) {}

fn void test2(int b = FOO~) {}