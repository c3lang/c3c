import std;

interface Foo
{
	fn int write(int bar);
}

fn void test2()
{
	int* b;
	$typeof(Foo.write) c = b; // #error:  You cannot cast 'int*' to 'fn int(void*,int)'
}
fn void test()
{
	void* b = &Foo.write; // #error:  You may not take the address of an interface method
}
fn int main()
{

	Foo.write(null, 1); // #error: An interface method must be called as a method, it can't be used as a free function
	return 0;
}