
macro @foo(; @body(int a, ...)) 
{
	@body(1, null, null);  // #error: You cannot cast 'void*' to 'float'.
}

fn void main()
{
	@foo(; int a, float b, void* c)
	{
			
	};
}
