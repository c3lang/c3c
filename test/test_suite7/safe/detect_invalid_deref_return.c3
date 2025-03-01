fn int[] test1()
{
	int[3] x;
	return &x; // #error: local variable will be invalid
}

fn int* test2()
{
	int[3] x;
	return &x[0]; // #error: local variable will be invalid
}

struct Abc
{
	int a;
	int[3] b;
}

fn int* test3()
{
	Abc x;
	return &x.a; // #error: local variable will be invalid
}

fn int* test4()
{
	Abc x;
	return &x.b; // #error: local variable will be invalid
}

fn int* test5()
{
	Abc x;
	return &x.b[0]; // #error: local variable will be invalid
}

fn int* test6()
{
	return &&1; // #error: temporary value will
}
