struct Test
{
	union a @tag("a", 1)
	{
		int x;
	}
}

fn int main(String[] args)
{
	$assert(Test.a.has_tagof("a")); // Error: Compile time assert failed.
	return 0;
}