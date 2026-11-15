macro @deferred(; @body()) { defer @body(); }

fn int foo()
{
	@deferred() { return 3; }; // #error: Return is not allowed inside of a defer
	return 1;
}
