module tlocal_addr_runtime;
import std::thread;

tlocal int[2] g_tlar;
int g_plain;
tlocal int* g_tl_plain = &g_plain;

fn void tlocal_addr_per_thread() @test
{
	int* main_ptr = &g_tlar[1];
	*main_ptr = 5;
	Thread t;
	t.create(fn int(void* arg) {
		int* p = &g_tlar[1];
		if (p == (int*)arg || *p != 0) return 1;
		*p = 7;
		if (g_tl_plain != &g_plain) return 3;
		return g_tlar[1] == 7 ? 0 : 2;
	}, main_ptr)!!;
	assert(t.join()!! == 0);
	assert(g_tlar[1] == 5 && main_ptr == &g_tlar[1]);
	assert(g_tl_plain == &g_plain);
}
