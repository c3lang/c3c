module const_pointer_diff;

fn void const_pointer_diff_negative() @test
{
	sz d = (int*)4 - (int*)16;
	assert(d == -3);
	const sz E = (long*)0 - (long*)8;
	assert(E == -1);
	assert((char*)1 - (char*)3 == -2);
}

fn void const_pointer_diff_positive() @test
{
	assert((int*)16 - (int*)4 == 3);
	assert((int*)16 - (int*)16 == 0);
	int* a = (int*)4;
	int* b = (int*)16;
	assert(a - b == (int*)4 - (int*)16);
}
