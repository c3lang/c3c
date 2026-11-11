// `??` with a void result evaluates one side and produces no value.
module else_void;

faultdef MALFORMED;
int hits;

macro void? check(int x)
{
	if (x < 0) return MALFORMED~;
	hits += 1;
}

fn void? check_fn(int x)
{
	if (x < 0) return MALFORMED~;
	hits += 10;
}

fn void note() => hits += 100;

fn void void_else() @test
{
	hits = 0;
	check(1) ?? (void)1;
	assert(hits == 1);
	check(-1) ?? note();
	assert(hits == 101);
	check_fn(1) ?? note();
	assert(hits == 111);
	check_fn(-1) ?? note();
	assert(hits == 211);
}
