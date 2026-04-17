module std::test;

macro foo() {}
fn void testa()
{
	$typeof(foo) a; // #error: A macro has no defined type.
}

fn void testb()
{
	@sizeof($$veccompeq); // #error: @sizeof only works on types with a definite size
}

enum Abc { ABC }
fn void testc()
{
	$typeof(Abc::from_ordinal) a; // #error: A type function has no defined type
}
fn void main()
{}
