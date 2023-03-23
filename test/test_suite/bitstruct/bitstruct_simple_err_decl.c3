bitstruct Foo : char
{
	bool a : 1;
	bool b; // #error: remove ranges from the other member
}

bitstruct Foo2 : char
{
	bool a;
	bool b : 1; // #error: ranges to all other members
}