
fn void test() @weak
{
}

fn int test() // #error: The definition for 'test' doesn't match the replaced definition, type mismatch between 'fn int()' and 'fn void()'
{
	return 0;
}
