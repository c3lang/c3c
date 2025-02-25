fn void int_min() @test
{
	assert(int.min == -2147483648);
	assert((float)int.min == -2147483648.0f);
	assert(short.min == -32768);
	assert((float)short.min == -32768.0f);
	assert(ichar.min == -128);
	assert((float)ichar.min == -128.0f);
}
