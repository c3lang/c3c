
excuse {EXCUSE}
fn int? fail() => EXCUSE~;

macro @foo(; @body(...)) 
{
	@body(fail()); // #error: Optional arguments are not permitted in a body invocation.
}

fn void main()
{
	@foo(; a)
	{
			
	};
}
