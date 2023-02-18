bitstruct Foo : char
{
	bool a : 1;
	bool b; // #error: Expected a ':'
}

bitstruct Foo2 : char
{
	bool a;
	bool b : 1; // #error: Expected ';'
}