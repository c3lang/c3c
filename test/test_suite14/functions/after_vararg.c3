fn void test1(int... x, int) { } // #error: A parameter name was expected
fn void test2(x..., int) { } // #error: A parameter name was expected
extern fn void test3(int a, ..., int y); // #error: C-style varargs cannot be followed by regular parameters.