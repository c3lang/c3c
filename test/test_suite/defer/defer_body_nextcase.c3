macro @deferred(; @body()) { defer @body(); }

fn void foo(int x)
{
	switch (x)
	{
		case 0:
			@deferred() { nextcase 1; }; // #error: No matching switch could be found.
		case 1:
			break;
	}
}
