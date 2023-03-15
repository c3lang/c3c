module faults @test;

fault Foo
{
	ABC,
	CDE
}

fn void ordinals()
{
	Foo z = null;
	assert(z.ordinal == 0);
	$assert(Foo.ABC.ordinal == 1);
	$assert(Foo.CDE.ordinal == 2);
	$assert(((Foo)null).ordinal == 0);
	Foo x = Foo.CDE;
	assert(x.ordinal == 2);
	x = Foo.ABC;
	assert(x.ordinal == 1);
}

