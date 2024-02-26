enum Test : {int a, int b} int
{
	FOO = {1, 2}
}

enum Test2 : {int a, int nameof} int  // #error: 'nameof' is not a valid parameter name for enums
{
	FOO = {1, 2}
}
