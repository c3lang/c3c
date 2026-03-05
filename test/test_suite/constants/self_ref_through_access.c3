struct Abc
{
	int a;
	int b;
}
const Abc X = { 2, Y };
const Y = X.b; // #error: This looks like the initialization of the variable was circular