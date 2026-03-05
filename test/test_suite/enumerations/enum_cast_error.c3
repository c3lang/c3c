enum Abc : char { ABC }

fn void foo()
{

	Abc x = (Abc)10; // #error: exceeds the max
}

fn void bar()
{
	int a;
	Abc x = (Abc)a;
}

fn void baz()
{
	int a;
	Abc x = (Abc)0;
}

fn void abc()
{
	int a;
	Abc x = (Abc)-1; // #error: negative number
}