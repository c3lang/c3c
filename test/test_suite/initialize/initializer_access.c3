module test;
struct Abc
{
	int[2] a;
	int b;
}
fn int main()
{
	Abc a = { .a = {} .b = 3 }; // #error: A comma or '}' was expected here.
}
