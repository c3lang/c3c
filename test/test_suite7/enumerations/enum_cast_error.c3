enum Abc : char { ABC }

fn void foo()
{
	Abc x = Abc.from_ordinal(10); // #error: exceeds the max
}

fn void bar()
{
	int a;
	Abc x = Abc.from_ordinal(a);
}

fn void baz()
{
	int a;
	Abc x = Abc.from_ordinal(0);
}

fn void abc()
{
	int a;
	Abc x = Abc.from_ordinal(-1); // #error: negative number
}