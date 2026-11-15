macro @m(;@body(int& a)) // #error: Body parameters cannot be '&' parameters
{
	int z = 1;
	@body(z);
}

fn void main()
{
	@m(; x) { x = 4; };
}
