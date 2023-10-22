module castable @test;

fn void assignable()
{
	assert($assignable(12.0, int) == false);
	assert($assignable(12, int));
	assert(!$assignable("12", int));
	assert($assignable("12", String));
	assert($assignable("12", char*));
	assert($assignable("12", char[*]));
	assert($assignable("12", char[2]));
	assert($assignable("12", char[3]));
}

fn void castable()
{
	assert($defined((int)12.0));
	assert($defined((int)12));
	assert(!$defined((int)"12"));
	assert($defined((String)"12"));
	assert($defined((char*)"12"));
	assert($defined((char[2])"12"));
	assert($defined((char[3])"12"));
}