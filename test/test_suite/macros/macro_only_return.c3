macro @only_return() { return; }
macro void @only_return_void() { return; }

fn void call_untyped()
{
	@only_return();
}

fn void call_typed()
{
	@only_return_void();
	$assert $Typeof(@only_return())::name == "void";
}

fn int missing_return() // #error: Missing return statement
{
	@only_return();
}
