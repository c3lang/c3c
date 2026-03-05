module castable @test;

fn void assignable()
{
	assert(!$defined(int x = 12.0));
	assert($defined(int x = 12));
	assert(!$defined(int x = "12"));
	assert($defined(String x = "12"));
	assert($defined(char* x = "12"));
	assert($defined(char[*] x = "12"));
	assert($defined(char[2] x = "12"));
	assert($defined(char[3] x = "12"));
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