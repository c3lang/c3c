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

// A parse error abandons the rest of the function body,
// so every case needs a function of its own.

const Foo GLOBAL = {
	.a = 1,
	.a = 2, // #error: assigned twice
};

fn void adjacent()
{
	Foo f = { .a = 1, .a = 2 }; // #error: assigned twice
}

fn void apart()
{
	Foo f = { .a = 1, .b = 2, .a = 3 }; // #error: assigned twice
}

fn void union_field()
{
	Bar b = { .x = 1, .x = 2 }; // #error: assigned twice
}

fn void vector_field()
{
	int[<3>] v = { .x = 1, .x = 2 }; // #error: assigned twice
}

fn void splat_first()
{
	Foo base = {};
	Foo f = { ...base, .a = 1, .a = 2 }; // #error: assigned twice
}

fn void splat_apart()
{
	Foo base = {};
	Foo f = { ...base, .a = 1, .b = 2, .a = 3 }; // #error: assigned twice
}

fn void nested_path()
{
	Outer o = { .foo.a = 1, .foo.a = 2 }; // #error: assigned twice
}

fn void nested_list()
{
	Outer o = { .foo = { .a = 1, .a = 2 } }; // #error: assigned twice
}

fn void nested_field_and_index()
{
	Outer o = { .arr[1] = 1, .arr[1] = 2 }; // #error: assigned twice
}

fn void array_index()
{
	int[4] a = { [0] = 1, [0] = 2 }; // #error: assigned twice
}

fn void array_apart()
{
	int[4] a = { [0] = 1, [1] = 2, [0] = 3 }; // #error: assigned twice
}

fn void array_spelling()
{
	int[4] a = { [1] = 1, [0x1] = 2 }; // #error: assigned twice
}

fn void array_range()
{
	int[4] a = { [0..2] = 1, [0..2] = 2 }; // #error: assigned twice
}

fn void index_then_field()
{
	Foo[3] a = { [0].a = 1, [0].a = 2 }; // #error: assigned twice
}

fn void index_then_field_apart()
{
	Foo[3] a = { [0].a = 1, [1].a = 2, [0].a = 3 }; // #error: assigned twice
}

fn void index_then_nested_field()
{
	Outer[2] a = { [0].foo.a = 1, [0].foo.a = 2 }; // #error: assigned twice
}

fn void index_field_index()
{
	Outer[2] a = { [1].arr[2] = 1, [1].arr[2] = 2 }; // #error: assigned twice
}

fn void multi_dimensional()
{
	int[3][3] a = { [0][1] = 1, [0][1] = 2 }; // #error: assigned twice
}

fn void range_then_field()
{
	Foo[3] a = { [0..1].a = 1, [0..1].a = 2 }; // #error: assigned twice
}

fn void only_the_first_duplicate_in_a_list_is_reported()
{
	Foo f = {
		.a = 1,
		.a = 2, // #error: assigned twice
		.b = 3,
		.b = 4,
	};
}
