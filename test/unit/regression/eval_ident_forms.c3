module eval_ident_forms::inner;

const int BAZ = 42;
int baz = 7;
macro @twice(x) => x * 2;

module eval_ident_forms;

const int LOCAL_BAZ = 3;
int local_baz = 4;

fn void path_forms() @test
{
	assert($eval("eval_ident_forms::inner::BAZ") == 42);
	assert($eval("eval_ident_forms::inner::baz") == 7);
	assert($eval("inner::BAZ") == 42);
	assert($eval("inner::@twice")(5) == 10);
}

fn void plain_forms() @test
{
	assert($eval("LOCAL_BAZ") == 3);
	assert($eval("local_baz") == 4);
	int _x1 = 9;
	assert($eval("_x1") == 9);
}

fn void defined_forms() @test
{
	$assert $defined($eval("inner::BAZ"));
	$assert $defined($eval("local_baz"));
	$assert !$defined($eval("inner::NOPE_NOT_HERE"));
	$assert !$defined($eval("nope_not_here"));
}
