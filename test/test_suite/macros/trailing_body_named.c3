macro @foo(; @body(int arg))
{
	@body(arg: 0); // #error: Named arguments are not supported for body parameters
}

fn void main()
{
	@foo(; int arg) {};
}