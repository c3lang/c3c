struct Inner { char c; long l; }

fn void test()
{
	Inner[2] local;
	static long* f = &local[1].l; // #error: must be a constant
}
