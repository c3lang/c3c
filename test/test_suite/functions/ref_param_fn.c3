fn String str(int& element) // #error:  Ref parameters are only allowed on methods
{
	return "abc";
}

fn String int.str(int& element) // #error: A ref parameter should always be untyped
{
	return "abc";
}
