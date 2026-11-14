module x64_abi_padding_only_eightbyte;

// The second eightbyte of `Outer` is only padding, which used to crash the x64 ABI classification.
struct Inner { float a; }
struct Outer @align(16) { Inner i; }

fn float take(Outer o) => o.i.a;

fn Outer make(float f)
{
	Outer o;
	o.i.a = f;
	return o;
}

fn void x64_abi_padding_only_eightbyte() @test
{
	Outer o;
	o.i.a = 1.5f;
	assert(take(o) == 1.5f);
	assert(take(make(2.5f)) == 2.5f);
}
