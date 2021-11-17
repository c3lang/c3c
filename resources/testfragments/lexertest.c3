module foo;

const int = 0;

struct Boo
{
	int i;
	union
	{
		int death;
	};
}

fn void test()
{
	int i = 0;
	i++;
	if (i < 100)
	{
		int j = 0;
		j += i;
		return;
	}
}