module test;
alias Func = fn void (bool);

fn bool foo (String) => true;
fn void bar(Func func) => func(false);

fn void main()
{
	Func funcX = &foo; // #error: Implicitly casting
}

fn void test2()
{
	bar(&foo); // #error: Implicitly casting
}

fn void test3()
{
	Func func = fn bool (String) { return true; }; // #error: which doesn't match
}

fn void test4()
{
	bar(fn bool (String) { return true; }); // #error: which doesn't match
}