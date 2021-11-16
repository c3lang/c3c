module foo;

int oefk = 1;
int bob = 'HELO';

typedef Foo* as Bob;

fn void testdefer(int x)
{
    defer printf("A");
    defer
    {
        int x = 2;
        printf("B");
        x = x + 1;
    }
    //defer catch printf("B")
    //defer catch (error e) printf("%s", @name(e));
    //if (x = 1) return throw Error.FOO;
    printf("!");
}

struct Foo
{
	int i;
	Bar *x;
}

struct Bar
{
	Foo foo;
	Foo* fooPtr;
	Bob helo;
	struct bar
	{
		int a;
	}
}

union Xyz
{
	int x;
	int y;
}


$if (1)
{
	struct Ogg
	{
		Zed *bob;
	}
	struct DOgg
	{
		Zed bob;
	}
	struct Zed
	{
		Foo b;
	}

}


struct Simple
{
	int i;
	double j;
}
fn int boo()
{
	Zed zfe;
	Simple s = { 1 };
	s = { 2 };
	//Simple x = { j = 1 };
	//Zed zfed = { 1 };
	int eok = 0;
	{
		eok = 2;
	}
	eok = eok * 2 + 1 * 2;
	eok = eok / 2 + 32 / 2;
	eok = eok % 2 - 32 % 45;

	int de = eok;
	de++;
	de = de << 1;
	de = de >> 1;
	de = de ^ (byte)(1);
	de = de & 1;
	de = de | 1;
	if (de > 100 || de < 10 || de <= 1000 || de >= 1921 || de == 100 || de != 2)
	{
		return boo();
	}
	switch (eok)
	{
		case 1:
			eok++;
			next;
		case 3:
			eok++;
			next;
			eok--;
		case 2:
			eok--;
		default:
			eok--;
			break;
	}


		if (eok > 0)
	{
		eok++;
	}
	else
	{
		eok--;
	}
	int xfeef = -eok;
	/*
	bool xoek = !eok;
	int oekfefo = ~eok;
	int *booe = &eok;
	int **de = &booe;
	int &xfok = &eok;
	int &&bo = &xfok;
	int e12 = **bo;
	int fe = *(&eok);
	int x2 = *xfok;*/
	int x1 = 2;
	FOO:
	x1 = x1 + 1;
	goto FOO;
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

fn int helo(int i)
{
	return i + 1;
}
fn void while_test()
{

	int a = 10;

	while (int b = 37; a < 0)
	{
		a++;
		int xy = 1;
	}
}

fn void test()
{
	int a = 10;
	while (1)
	{
		int xy = 1;
	}
	int eokfe = boo();
	int i = -1;
	bool dwf = !2.0;
	ushort b = ~(byte)(0);
	int j as k;
	int l as m = 0;
	int o = 0, p = 3;
	short f = (char)((int)(-2));
	//int doek = ~2;
	return;
}

fn void main()
{
	helo(2);
	printf("Helo\n");
}