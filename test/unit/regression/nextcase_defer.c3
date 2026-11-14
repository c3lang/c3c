module nextcase_defer;

fn int jump_nextcase_reads_before_defer()
{
	int x = 0;
	int visits = 0;
	switch (x) @jump
	{
		case 0:
			visits++;
			if (visits > 1) break;
			defer x = 2;
			nextcase x;
		case 2:
			visits += 100;
	}
	return visits;
}

fn int plain_nextcase_reads_before_defer()
{
	int x = 0;
	int visits = 0;
	switch (x)
	{
		case 0:
			visits++;
			if (visits > 1) break;
			defer x = 2;
			nextcase x;
		case 2:
			visits += 100;
	}
	return visits;
}

fn void nextcase_value_is_read_before_defers() @test
{
	assert(plain_nextcase_reads_before_defer() == 2);
	assert(jump_nextcase_reads_before_defer() == 2);
}
