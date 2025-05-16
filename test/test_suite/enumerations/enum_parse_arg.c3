enum Foo : (int x, int y)
{
	ABC = { 1 2 } // #error: A comma or a closing brace
}
