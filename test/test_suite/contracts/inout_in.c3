<*
 @param [in] foo
*>
fn void test(int* foo)
{
	test2(foo); // #error: An 'in' parameter may not be passed into
}

<*
 @param [out] foo
*>
fn void test3(int* foo)
{
	test2(foo); // #error: An 'out' parameter may not be passed into
}
<*
 @param [inout] foo
*>
fn void test2(int* foo)
{
}
