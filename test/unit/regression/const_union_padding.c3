module const_union_padding @test;

union Un { char c; long l; }
struct Bx { Un u; short s; }

Bx[2] g_arr = { { { .c = 1 }, 2 }, { { .c = 3 }, 4 } };
Bx g_single = { { .c = 1 }, 2 };
short g_after = 77;

fn void const_array_stride()
{
	assert(g_arr[0].u.c == 1);
	assert(g_arr[0].s == 2);
	assert(g_arr[1].u.c == 3);
	assert(g_arr[1].s == 4);
}

fn void const_local_copy()
{
	Bx[2] local = { { { .c = 1 }, 2 }, { { .c = 3 }, 4 } };
	assert(local[1].u.c == 3);
	assert(local[1].s == 4);
}

fn void const_global_size()
{
	Bx other = { { .l = -1 }, -1 };
	g_single = other;
	assert(g_after == 77);
}
