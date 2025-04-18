fn void foo(int a, int b, bool c = false) {}
fn void foo2(int a, int b, bool c) {}

fn int main()
{
	foo(2); // #error: 1-2 additional arguments were expected
	foo2(2);// #error: 2 more arguments
	foo();  // #error: expects 2-3 parameters
	foo2(); // #error: expects 3 parameter(s)
	return 0;
}