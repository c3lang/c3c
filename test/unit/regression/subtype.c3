module switch_subtype @test;

struct Foo
{
	inline Baz g;
	int a;
}
struct Bar
{
	inline Foo f;
}
struct Baz
{
	int z;
}
fn void switch_subtyping()
{
	Foo f = {};
	f.z = 123;
	any x = &f;
	switch (x)
	{
		case int:
			assert(false);
		case Baz:
			assert(true);
		default:
			assert(false);
	}
	switch (x)
	{
		case int:
			assert(false);
		case Foo:
			assert(true);
		default:
			assert(false);
	}
	switch (x)
	{
		case int:
			assert(false);
		case Bar:
			assert(false);
		default:
			assert(true);
	}
	switch (Bar.typeid)
	{
		case int:
			assert(false);
		case Baz:
			assert(true);
		case Foo:
			assert(false);
		case Bar:
			assert(false);
		default:
			assert(false);
	}
	switch (Bar.typeid)
	{
		case int:
			assert(false);
		case Foo:
			assert(true);
		case Bar:
			assert(false);
		default:
			assert(false);
	}
	$switch (Bar.typeid)
	$case Foo:
		assert(true);
	$case Bar:
		assert(false);
	$default:
		assert(false);
	$endswitch
}

fn void is_subtype()
{
	$assert(types::is_subtype_of(Bar, Baz));
	$assert(!types::is_subtype_of(Baz, Bar));
	typeid baz = Baz.typeid;
	typeid bar = Bar.typeid;
	assert(bar.is_subtype_of(baz));
	assert(!bar.is_subtype_of(double.typeid));
	assert(!baz.is_subtype_of(bar));
}
