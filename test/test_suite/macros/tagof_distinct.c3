macro @foo(#type)
{
	return $Typeof(#type)::has_tag("hello");
}

typedef Foo @constinit = uint;

fn int main()
{
	Foo f = 1;
	@foo(f);
	return 0;
}
