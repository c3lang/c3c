module defer_nextcase_copy;

int inner_runs;
int cases_seen;

fn int two_exits(int r)
{
	defer
	{
		switch (r)
		{
			case 0:
				defer inner_runs++;
				cases_seen += 1;
				nextcase 1;
			case 1:
				cases_seen += 10;
		}
	}
	if (r) return 1;
	return 0;
}

fn void defer_switch_nextcase()
{
	defer switch (0)
	{
		case 0:
			defer inner_runs++;
			nextcase 1;
		case 1:
			cases_seen += 100;
	}
}

fn void nextcase_in_copied_defer() @test
{
	inner_runs = 0;
	cases_seen = 0;
	two_exits(0);
	assert(inner_runs == 1 && cases_seen == 11);
	two_exits(1);
	assert(inner_runs == 1 && cases_seen == 21);
	defer_switch_nextcase();
	assert(inner_runs == 2 && cases_seen == 121);
}
