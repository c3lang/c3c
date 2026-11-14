faultdef BAD;
fn int? bar() => BAD~;
macro @deferred(; @body()) { defer @body(); }

fn int? foo()
{
	@deferred() { bar()!; }; // #error: Rethrows are not allowed inside of defers.
	return 1;
}
