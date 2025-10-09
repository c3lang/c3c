import std;

macro @_macro(#i)
{
	if (#i == 0) { return; }
	return @_macro(#i - 1); // #error: Failure evaluating macro, max call depth reached
}

fn void main()
{
	@_macro(1);
}
