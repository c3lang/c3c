module distinct_inline @test;

distinct Foo = inline int;
distinct Bar = inline Foo;
distinct Baz = inline Foo;
distinct Abc = inline Baz;
distinct Def = inline Abc;
distinct Other = inline int;
distinct Other2 = inline Other;
fn void test()
{
	assert($typeof((Foo)1 + 1).typeid == Foo.typeid);
	assert($typeof((Foo)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Baz)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Def)1 + (Bar)1).typeid == Foo.typeid);
	assert($typeof((Other2)1 + (Def)1).typeid == int.typeid);
}