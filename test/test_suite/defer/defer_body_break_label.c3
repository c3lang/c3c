macro @deferred(; @body()) { defer @body(); }

fn void foo()
{
	while FOO: (true)
	{
		@deferred() { break FOO; }; // #error: You cannot break out of a defer.
	}
}
