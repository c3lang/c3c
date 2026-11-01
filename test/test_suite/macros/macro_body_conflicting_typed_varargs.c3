
macro @foo(; @body(int a, void* ...)) 
{
	@body(1, null, null);
}

fn void main()
{
	@foo(; int a, int b, void* c) // #error: This parameter should be 'void*' but was 'int'
	{
			
	};
}
