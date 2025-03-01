module inlineme;

<*
 @pure
 *>
fn void test()
{
	int x = 123;
	int* y = &x;
}

int abc;
fn void test2()
{
	abc = 1233;
}

<* @pure *>
fn void test3()
{
	abc = 1233; // #error: '@pure' functions may not access globals
}