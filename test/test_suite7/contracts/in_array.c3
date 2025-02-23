<*
 @param [in] x
*>
fn void test(int* x)
{
	x[1] = 123; // #error: 'in' parameters may not be assigned to
}
fn int main()
{
	test(&&int[3]{ 1, 2, 1 });
	return 0;
}