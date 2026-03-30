macro @foo(#type)
{
	return $typeof(#type).has_tagof("hello");
}

typedef Foo @constinit = uint;

fn int main()
{
	Foo f = 1;
	@foo(f);
	return 0;
}
