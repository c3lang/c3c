import std;

macro @_macro(#i)
{
	if (#i == 0) { return; }
	return @_macro(--#i); // #error: You cannot assign to a constant expression
}

fn void main()
{
	@_macro(1);
}