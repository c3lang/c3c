
macro @foo(; @body(int a, int ...)) 
{
	const int[] INTEGERS = {1, 2, 3};
	@body(1, ... INTEGERS);
}

macro @bar(...; @body(...)) 
{
	@body(... $vaarg);
}

fn void main()
{
	@foo(; a, b, c, d) { };
	@bar(1, 2, 3; a, b, c) {};
}
