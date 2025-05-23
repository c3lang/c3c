import std;

fn void main()
{
	@foo(;int x)
	{
		io::printn(x);
	};
}

macro @foo(; @body(int x = 1)) // #error: Body parameters
{
	@body(3);
}
