struct Foo
{
	int a;
	int b;
	int c;
}

struct Outer
{
	Foo foo;
	int[4] arr;
}

union Bar
{
	int x;
	float y;
}

const int ZERO = 0;

// Only designators that are literally identical are rejected. Everything below
// either overwrites an earlier value through a different path, or names the
// same element in a way that is not compared, and must keep compiling.
fn void different_elements()
{
	Foo base = {};
	Foo splat_override = { ...base, .a = 1 };
	Foo splat_all = { ...base, .a = 1, .b = 2, .c = 3 };
	Outer same_names_in_different_lists = { .foo = { .a = 1 }, .arr = { [0] = 1 } };
	Foo[2] separate_lists = { { .a = 1 }, { .a = 2 } };
	Outer a_then_b = { .foo.a = 1, .foo.b = 2 };
}

fn void overwrite_through_other_path()
{
	Outer parent_then_child = { .foo = { .a = 1, .b = 2 }, .foo.a = 3 };
	Outer child_then_parent = { .foo.a = 3, .foo = { .a = 1, .b = 2 } };
	Bar union_members = { .x = 1, .y = 2.0f };
}

fn void overwrite_arrays()
{
	int[4] range_then_index = { [0..3] = 1, [2] = 5 };
	int[4] index_then_range = { [2] = 5, [0..3] = 1 };
	int[4] different_ranges = { [0..1] = 1, [0..2] = 2 };
	int[4] different_range_start = { [0..2] = 1, [1..2] = 2 };
	int[4] range_then_same_start = { [0..2] = 1, [0] = 2 };
	int[<3>] swizzle_then_single = { .xy = 1, .x = 2 };
}

// Longer paths are only equal if every element is equal.
fn void paths_differing_in_one_element()
{
	Foo[3] other_field = { [0].a = 1, [0].b = 2 };
	Foo[3] other_index = { [0].a = 1, [1].a = 2 };
	Foo[3] index_then_range = { [0].a = 1, [0..1].a = 2 };
	Foo[3] whole_then_field = { [0] = { .a = 1 }, [0].a = 2 };
	int[3][3] other_inner_index = { [0][1] = 1, [0][2] = 2 };
	int[3][3] other_outer_index = { [0][1] = 1, [1][1] = 2 };
	Outer other_array_index = { .arr[1] = 1, .arr[2] = 2 };
	Outer[2] other_nested_field = { [0].foo.a = 1, [0].foo.b = 2 };
	Outer[2] other_inner_array_index = { [0].arr[1] = 1, [0].arr[2] = 2 };
	Outer[2] other_outer_array_index = { [0].arr[1] = 1, [1].arr[1] = 2 };
}

fn void not_literal()
{
	int[4] named_constant = { [0] = 1, [ZERO] = 2 };
	int[4] expression = { [1] = 1, [0 + 1] = 2 };
	int[4] two_constants = { [ZERO] = 1, [ZERO] = 2 };
}

fn void explicit_override()
{
	Foo eval = { .a = 1, .$eval("a") = 2 };
	Foo eval_twice = { .$eval("a") = 1, .$eval("a") = 2 };
}
