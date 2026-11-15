module macro_lambda_ct_type;

alias MlFn = fn long();

macro square($x) { return fn long() => (long)($x * $x); }
macro type_name($x) { return fn String() => $Typeof($x)::name; }

fn void lambda_differs_by_ct_arg_type() @test
{
	MlFn a = square(100000);
	MlFn b = square(100000L);
	assert(a() == 1410065408);
	assert(b() == 10000000000);
}

fn void lambda_name_differs_by_ct_arg_type() @test
{
	assert(type_name(1)() == "int");
	assert(type_name((char)1)() == "char");
	assert(type_name(1L)() == "long");
}

fn void lambda_same_ct_arg_reused() @test
{
	MlFn a = square(3);
	MlFn b = square(3);
	assert(a == b);
	assert(a() == 9);
}
