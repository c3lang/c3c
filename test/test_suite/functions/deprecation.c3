
fn int foo() @deprecated
{
	if (false) foo();
	return 1;
}

fn int main() @allow_deprecated
{
	foo();
	return 0;
}

fn int foo2() @deprecated
{
	return 1;
}

fn int main2()
{
	foo2(); // #warning: 'foo2' is deprecated
	return 0;
}