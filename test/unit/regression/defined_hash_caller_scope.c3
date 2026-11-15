module defined_hash_caller_scope;

struct DhFoo { int a; }

macro bool @is_defined(#e)
{
	int y;
	return $defined((void)#e);
}

macro bool @outer(...) => @is_defined($vaarg[0]);

fn void defined_hash_plain_uses_caller_scope() @test
{
	DhFoo y = { 1 };
	assert(@is_defined(y.a));
}

fn void defined_hash_compound_literal_uses_caller_scope() @test
{
	DhFoo y = { 1 };
	assert(@is_defined((DhFoo){ y.a }));
}

fn void defined_hash_reflect_uses_caller_scope() @test
{
	DhFoo y = { 1 };
	assert(@is_defined($reflect(y.a)));
}

fn void defined_hash_vaarg_uses_caller_scope() @test
{
	DhFoo y = { 1 };
	assert(@outer(y.a));
}

macro bool @is_defined_plain(#expr) => $defined((void)#expr);
macro bool @forward_plain(...) => @is_defined_plain($vaarg[0]);

fn void defined_hash_forwarded_vaarg() @test
{
	int value = 1;
	assert(@forward_plain(value));
}

macro bool @forward_second(...) => @is_defined_plain($vaarg[1]);
macro bool @direct_second(...) => $defined($vaarg[1]);
macro bool @direct_second_cast(...) => $defined((void)$vaarg[1]);
macro bool @direct_from_end(...) => $defined($vaarg[^3]);

fn void defined_vaarg_out_of_range_is_false() @test
{
	int value = 1;
	assert(!@direct_second(value));
	assert(!@direct_second_cast(value));
	assert(!@forward_second(value));
	assert(!@direct_from_end(value, value));
	assert(@direct_second(value, value));
	assert(@forward_second(value, value));
	assert(@direct_from_end(value, value, value));
}
