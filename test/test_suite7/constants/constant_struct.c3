import std::io;

struct Abc
{
	int a;
}

struct Bcd
{
	Abc x;
}

const Abc FOO = { 2 };

fn void main()
{
	Bcd a = { FOO };
}
