struct Test
{
	union a @tag("a", 1)
	{
		int x;
	}
}

fn int main(String[] args)
{
	$assert($reflect(Test.a).has_tag("a")); // Error: Compile time assert failed.
	return 0;
}