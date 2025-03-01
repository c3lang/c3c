
macro void @foo(int #a)
{
	var x = float.#a; // #error: cannot already be resolved
}

fn void main()
{
	int inf;
	@foo(inf);
}