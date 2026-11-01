
macro @foo(; @body(int a, void* ...)) 
{
	@body(1, null, 12.0);  // #error: You cannot cast 'double' to 'void*'
}

fn void main()
{
	@foo(; int a, b, c)
	{
			
	};
}
