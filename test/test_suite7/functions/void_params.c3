fn void test(int, void) {} // #error: Parameters may not be of type 'void'.
extern fn void test3(void); // #error: C-style 'foo(void)' style argument declarations are not valid