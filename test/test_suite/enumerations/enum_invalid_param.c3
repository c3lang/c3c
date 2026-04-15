enum Test : int (int kindof, int qname)
{
	FOO {1, 2}
}

enum Test2 : (int a, int description)  // #error: 'description' is not a valid parameter name for enums
{
	FOO {1, 2}
}

enum Test3 : (int a, int ordinal)  // #error: 'ordinal' is not a valid parameter name for enums
{
	FOO {1, 2}
}
