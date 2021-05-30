module foo <$foo, #bar, Integer>;
import bar as eok local;
import bar2 as eok2;
import bar3 local;

macro void @foo(int i, $e)
{
    $e = 1;
    printf("Helo");
}
macro @goo(i, $e)
{

}

struct Foo
{
    $if ($use_bar > 0)
    {
        int bar;
    }
    int baz;
}


macro @soom!(i, $e)
{}

local struct Foom
{
	int i;
	Foom *test;
	int*** j;
	int*[][]* k;
}

struct Hej
{
	int x;
}

enum FEok : int {
	IFEJ
}


enum Test
{
    FOO = 1 + 2,
	BAR,
}

enum Test2 : int
{
	FOO = 1,
	BAR,
}

union Foomt
{
	int i;
	double d;
}

error Errors
{
	BADERROR,
	OTHER_ERROR
}

func Foom test(int a)
{
	while (int x = 0, int y = 3; int y = foo())
	{
		a++;
	}
	while (int x = 0)
	{
		a++;
	}
	while (int x, y = 3; x > 0 && b < 0)
	{
		a++;
	}
	return 1 + 2;
}

func boo::Bar zab::Baz.sd(die::Eij i) throws Zab // , sij:Zig
{
	float a = 0, b = 3, double c = 1, d;
	int i = 0;
}

generic int boor(i)
{
    case int:
        return 1;
    case double:
        return 100;
    default:
        return 1000;
}

generic boor2(i)
{
    case int:
        return "Helo";
    default:
        return 10001;
}

$if ($e > 0)
{
    func void foo() {}
}
$elif ($e < 0)
{
    func void foo() { printf("HELO"); }
}
$else
{
    func void foo() { printf("OLEH"); }
}

$if ($e > 0)
{
    func void foo() {}
}

$if ($b > 0)
{
}
$else
{
    generic test(i) { }
}

#if $b > 0

#else

#endif

generic boofer2(i, g, eok)
{
    case int, char[], type($eoo):
        return "Helo";
    default:
        return 1000;
}



func void hello() throws Errors
{
	int i, j;
	throw FOO;
	throw awesome::FOO;
	defer close(b);
	foo::Bar x = 3;
	try foo();
	try foo() else 1;
	foo(try 1);
    type($error) fk;
	type(int).size + fk;
	Errors {};
	Ferrors{a = 1, b = 20, b = { token }};
	Ferrors{1, 3, 1+4};
	$erro = 1;
	FOO:
	goto FOO;
	type($error) fk;
	foo::@macrof();
	int i = foo ? 2 : 4;
	@macros();
	type(foo::y) z;
	type(int) * 2;
	$error = type(int);
	double[4] a;
	foo[1 + 2] * b;
	type((i > 0) ? type(int) : type(double)) doek;
	$e = type(type(type(Bar)));
	$e = foo ? type(int) : type(Bar);
	$e = type(type(foo::$eofk));
	if (a == 0 && 1 == b)
	{
		i = 0;
	}
	while (bpb >= 3)
	{
	    a();
	}
	do
	{
	} while (0);
	for (i = 0;;)
	{}
	for (i = 0, j = 3; i < 0; i++, j++) {}
	for (int i = 0; i < 100; i++)
	{
	    i++;
	}
	int i = 1;
	i + 1 * 100;
	&i;
	int j = i;
	2;
	i++;
	switch (int foo = 1; bar)
	{
	    case 1:
	        next;
	        continue;
	    default:
	       break;
	}
	do {
		i++;
	} while (a < 0);
	while (a > 0)
	{
		a--;
	}
	while (int a = 4; int b = 20)
	{
	    a + 1;
	}
	return;
}

typedef Foo* as Bar;
typedef func void(int, Foo*) as Zoo;



func void test2()
{
	return;
}