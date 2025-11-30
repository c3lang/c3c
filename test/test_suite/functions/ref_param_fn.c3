fn String str(int& element) // #error: Self parameters are only allowed on methods
{
	return "abc";
}

fn String int.str(int& element) // #error: A self parameter should always be untyped, please remove the type here
{
	return "abc";
}
