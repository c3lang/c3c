module test;
struct Foo
{
	float val;
	int ival;
}

fn int main()
{
	Foo bar;
	bar.val /= 0f;
	bar.ival /= 0; // #error: Division by zero not allowed
	return 0;
}
