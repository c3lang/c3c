
macro @foo(; @body(int a, ...)) 
{
	@body(1, 2, 3, 4, null);  // #error: This argument would exceed the number of parameters, did you add too many arguments?
}

fn void main()
{
	@foo(; a, b)
	{
			
	};
}
