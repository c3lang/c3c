enum Test : int (int a, int b)
{
	FOO = {1, 2}
}

enum Test2 : (int a, int nameof)  // #error: 'nameof' is not a valid parameter name for enums
{
	FOO = {1, 2}
}
