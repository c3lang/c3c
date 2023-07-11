module test;

fn void test(int* a) {}
macro @abc(&self)
{
	test(self); // #error: is a dereferenced pointer
}

fn void! test1()
{
	int* a;
	int b;
	@abc(a);
	@abc(b);
}
