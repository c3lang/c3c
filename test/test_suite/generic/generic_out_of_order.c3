module c3test;

import std::collections::list;

$expand(@makefn(Foo));

struct Foo @tag("test", true)
{
	List{uint} a; // #error: Instantiation of the generic type 'List' happens before methods are registered, please try to rearrange the instantiation order.
}

fn int main()
{
	Foo v;
	testfn(&v);
	return 0;
}

macro String @makefn($Type) {
	$Type::has_tag("test");
	return `fn bool testfn(Foo* v) {
return v.a.is_initialized();
}`;
}