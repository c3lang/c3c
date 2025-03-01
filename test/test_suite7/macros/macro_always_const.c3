macro bar() @const
{
	return; // #error: provide a value
}

macro void barz() @const // #error: return a constant
{
	return 1;
}
