module loop_decl_cond;

fn int count_while()
{
	int n;
	while (bool b = true)
	{
		assert(b);
		if (++n == 3) break;
	}
	return n;
}

fn int count_for()
{
	int n;
	for (int i = 0; bool c = true; i++)
	{
		assert(c);
		if (++n == 2) break;
	}
	return n;
}

fn void loop_decl_cond() @test
{
	assert(count_while() == 3);
	assert(count_for() == 2);
}
