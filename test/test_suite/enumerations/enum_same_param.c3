enum Test : (int a, int b)
{
	FOO = { 1, 2 }
}

enum Test2 : int (int a, int a)  // #error: Duplicate parameter name 'a'
{
	FOO = { 1, 2 }
}
