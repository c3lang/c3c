module foo;

const int GLOBAL = 0;

struct Boo
{
	int i;
	union
	{
		int death;
	};
}

func void test()
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