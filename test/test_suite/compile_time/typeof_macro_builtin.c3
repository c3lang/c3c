module std::test;

macro foo() {}
fn void testa()
{
	$typeof(foo) a; // #error: A macro has no defined type.
}

fn void testb()
{
	$sizeof($$veccompeq); // #error: A builtin function has no defined type
}

enum Abc { ABC }
fn void testc()
{
	$typeof(Abc.from_ordinal) a; // #error: A type function has no defined type
}
fn void main()
{}
