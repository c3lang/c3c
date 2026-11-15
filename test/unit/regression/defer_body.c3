module defer_body;

macro @deferred(; @body()) { defer @body(); }

fn void loops_inside_deferred_body() @test
{
	int sum;
	@deferred()
	{
		for (int i = 0; i < 10; i++)
		{
			if (i == 4) break;
			if (i == 1) continue;
			sum += i;
		}
	};
	assert(sum == 5);
}

fn void switch_inside_deferred_body() @test
{
	int x;
	@deferred()
	{
		switch (x)
		{
			case 0:
				x = 10;
				nextcase 1;
			case 1:
				x++;
		}
	};
	assert(x == 11);
}
