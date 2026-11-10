macro @deferred(; @body()) { defer @body(); }

fn void foo()
{
	while (true)
	{
		@deferred() { break; }; // #error: There is no valid target for 'break'
	}
}
