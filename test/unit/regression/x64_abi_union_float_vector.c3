module x64_abi_union_float_vector;

// A union with a float vector member was split wrongly into registers on x64.
union Foo { float[<3>] v; }
struct Bar { float[<3>] v; }

fn float sum_union(Foo f) => f.v[0] + f.v[1] + f.v[2];
fn float sum_struct(Bar b) => b.v[0] + b.v[1] + b.v[2];

fn Foo make(float[<3>] v)
{
	Foo f;
	f.v = v;
	return f;
}

fn void x64_abi_union_float_vector() @test
{
	Foo f;
	f.v = { 1, 2, 4 };
	assert(sum_union(f) == 7);
	assert(sum_union(make({ 8, 16, 32 })) == 56);
	Bar b = { { 1, 2, 4 } };
	assert(sum_struct(b) == 7);
}
