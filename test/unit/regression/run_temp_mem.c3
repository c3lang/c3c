module temp_mem;

fn void test_realloc() @test
{
	void* mem = tmalloc(10);
	for (int i = 1; i < 4096 * 32 * 16 * 2; i <<= 1)
	{
		mem = mem::trealloc(mem, i);
	}
	for (int i = 4096 * 32 * 16 * 2; i > 2; i >>= 1)
    {
    	mem = mem::trealloc(mem, i);
    	(void)tmalloc(1235);
    }
}