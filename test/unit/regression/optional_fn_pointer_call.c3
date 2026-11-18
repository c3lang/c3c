module optional_fn_pointer_call;

alias Fn = fn int(int);
faultdef BAD;

fn int twice(int x) => x * 2;

fn void call_local_optional_fn() @test
{
	Fn? f = &twice;
	int? d = f(3);
	assert(d!! == 6);
	Fn? g = BAD~;
	int? e = g(3);
	assert(@catch(e) == BAD);
}
