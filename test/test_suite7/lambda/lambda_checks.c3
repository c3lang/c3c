module test;
def Func = fn void (bool);

fn bool foo (String) => true;
fn void bar(Func func) => func(false);

fn void main()
{
	Func funcX = &foo; // #error: Implicitly casting
	bar(&foo); // #error: Implicitly casting

	Func func = fn bool (String) { return true; }; // #error: which doesn't match
	bar(fn bool (String) { return true; }); // #error: which doesn't match
}