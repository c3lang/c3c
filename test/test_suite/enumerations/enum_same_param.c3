enum Test : { int a, int b } int
{
	FOO = { 1, 2 }
}

enum Test2 : { int a, int a} int  // #error: Duplicate parameter name 'a'
{
	FOO = { 1, 2 }
}
