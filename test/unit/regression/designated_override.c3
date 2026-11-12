module designated_override;

struct OvFoo { int a; int[3] c; }
struct OvBar { OvFoo f; int z; }

OvFoo g_foo = { .c = { 1, 2, 3 }, .c[2] = 7 };
int[4][2] g_matrix = { [1] = { 1, 2, 3, 4 }, [1][2] = 9 };

fn void override_element_after_const_array() @test
{
	OvFoo f = { .c = { 1, 2, 3 }, .c[2] = 7 };
	assert(f.c == { 1, 2, 7 });
}

fn void override_nested_array_after_const_array() @test
{
	int[4][2] m = { [1] = { 1, 2, 3, 4 }, [1][2] = 9 };
	assert(m[1] == { 1, 2, 9, 4 });
	assert(m[0] == { 0, 0, 0, 0 });
}

fn void override_after_const_array_global() @test
{
	assert(g_foo.c == { 1, 2, 7 });
	assert(g_matrix[1] == { 1, 2, 9, 4 });
}

fn void override_after_runtime_array() @test
{
	int y = 5;
	OvFoo f = { .c = { y, y, y }, .c[1] = 7 };
	assert(f.c == { 5, 7, 5 });
}
