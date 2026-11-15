tlocal int g_tls;

fn void test()
{
	tlocal int[3] t;
	int* ok = &g_tls;
	int* ok2 = &t[2];
	static int* p1 = &t[1]; // #error: must be a constant
}

fn void test2()
{
	static int* p2 = &g_tls; // #error: must be a constant
}
