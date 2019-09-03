module foo;

struct Foo
{
	int i;
	Bar *x;
}

struct Bar
{
	Foo foo;
	Foo* fooPtr;
}


func int boo()
{
	bool z = 123 > 3.0;
	{
		int x = 0;
	}
	{
		int x = 1;
	}
	int j = 10;
	do
	{
		j = j + 10;
	} while (j > 2);
	return 1;
}

func void while_test()
{

	int a = 10;
	while (int b = 37; a > 0)
	{
		int xy = 1;
	}
}

func void test()
{
	int a = 10;
	while (1)
	{
		int xy = 1;
	}
	int eokfe = boo();
	int i = -1;
	bool dwf = !2.0;
	ushort b = ~cast(byte, 0);
	int j, k;
	int l, m = 0;
	int o = 0, p = 3;
	short f = cast(byte, cast(int,-2));
	//int doek = ~2;
	return;
}

