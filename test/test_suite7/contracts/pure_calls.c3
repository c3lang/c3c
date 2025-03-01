module inlineme;

<*
 @pure
 *>
fn void test()
{
	int x = 123;
	int* y = &x;
	test3();
	test2() @pure;
	test2(); // #error: Only '@pure' functions may be called, you can override this with an attribute
}

int abc;
fn void test2()
{
	abc = 1233;
}

<* @pure *>
fn void test3()
{
}