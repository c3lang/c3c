
macro @foo(; @body(int ..., void* a))   // #error: Varargs cannot be followed by regular parameters in a macro body.
{
	@body(1, 2);
}

fn void main()
{
	@foo(; a, b)
	{
			
	};
}
