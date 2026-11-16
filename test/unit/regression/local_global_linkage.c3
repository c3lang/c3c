module local_global_linkage;

struct LglNode { LglNode* next; int value; }

LglNode a @local = { &b, 1 };
LglNode b @local = { &a, 2 };
tlocal int t @local = 3;
tlocal int* t_ptr @local = &b.value;
int? o @local;

fn void local_globals_refer_to_each_other() @test
{
	assert(a.next.next == &a);
	assert(a.next.value == 2);
	assert(*t_ptr == 2);
	t++;
	assert(t == 4);
}

fn void local_optional_global() @test
{
	assert(!@catch(o));
	o = 5;
	assert(o!! == 5);
}

fn void static_refers_to_unused_static() @test
{
	static int[2] unused;
	static int* p = &unused[1];
	*p = 7;
	assert(*p == 7);
}
