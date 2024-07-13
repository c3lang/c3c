enum Test : int (int kindof, int qnameof)
{
	FOO = {1, 2}
}

enum Test2 : (int a, int nameof)  // #error: 'nameof' is not a valid parameter name for enums
{
	FOO = {1, 2}
}

enum Test3 : (int a, int ordinal)  // #error: 'ordinal' is not a valid parameter name for enums
{
	FOO = {1, 2}
}
