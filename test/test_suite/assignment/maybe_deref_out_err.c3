module test;

<*
 @param [out] bar
*>
fn void foo(int[2]* bar)
{
	(*bar)[0] = 1;
	bar.[0]; // #error: 'out' parameters may not be read
}

fn int main()
{
	return 0;
}