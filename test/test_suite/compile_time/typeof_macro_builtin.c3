module std::test;

macro foo() {}
fn void testa()
{
	$typeof(foo) a; // #error: Taking the type of a macro is not supported.
}

fn void testb()
{
	$sizeof($$veccompeq); // #error: Taking the type of a builtin function is not supported
}

fn void main()
{}
