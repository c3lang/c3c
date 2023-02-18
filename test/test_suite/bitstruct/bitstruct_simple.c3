bitstruct Foo : char
{
	bool a;
	int b;  // #error: For bitstructs without bit ranges, the types must all be 'bool'
}

bitstruct Foo2 : char
{
	bool a0;
	bool a1;
	bool a2;
	bool a3;
	bool a4;
	bool a5;
	bool a6;
	bool a7;
	bool a8; // #error: overflow
}