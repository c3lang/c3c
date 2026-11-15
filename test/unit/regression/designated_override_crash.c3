module designated_override_crash;

struct OvcFoo { int a; int[3] c; }
struct OvcBar { OvcFoo f; int z; }

fn void override_range_after_const_array() @test
{
	OvcFoo f = { .c = { 1, 2, 3 }, .c[0..1] = 7 };
	assert(f.c == { 7, 7, 3 });
}

fn void override_member_path_after_const_array() @test
{
	OvcBar b = { .f.c = { 1, 2, 3 }, .f.c[0] = 5, .z = 1 };
	assert(b.f.c == { 5, 2, 3 });
	assert(b.z == 1);
}
