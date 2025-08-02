module list_test @test;
import std::collections::list;

alias IntList = List{int};
alias PtrList = List{void*};

struct Overalign
{
	float[<4>] x @align(128);
}

alias OveralignList = List{Overalign};
fn void overaligned_type()
{
	OveralignList l;
	defer l.free();

	Overalign y;
	for (int i = 0; i < 1000; i++) l.push(y);
	assert((usz)l.get_ref(2) - (usz)l.get_ref(1) == Overalign.sizeof);
}

fn void remove_at()
{
	IntList test;
	test.init(mem);
	defer test.free();
	test.add_array({ 1, 2, 3, 4 });
	test::eq(test.array_view(), (int[]){ 1, 2, 3, 4 });
	test.remove_at(0);
	test::eq(test.array_view(), (int[]){ 2, 3, 4 });
	test.remove_at(1);
	test::eq(test.array_view(), (int[]){ 2, 4 });
	test.remove_at(1);
	test::eq(test.array_view(), (int[]){ 2 });
	test.remove_at(0);
	test::eq(test.array_view(), (int[]){ });
}

fn void delete_contains_index()
{
	IntList test;

	test.add_array({ 1, 2 });
	assert(test.contains(1));
	assert(test.contains(2));
	assert(!test.contains(0));
	assert(!test.contains(3));
	assert(test.array_view() == { 1, 2 });
	test.push(3);
	assert(test.array_view() == { 1, 2, 3 });
	assert(test.contains(3));
	test[0] = 10;
	assert(test.contains(10));
	test.remove_item(10);
	assert(test.array_view() == { 2, 3 });
	assert(!test.contains(1));
	assert(test.contains(2));
	assert(test.len() == 2);
	test.push(0);
	test.insert_at(0, 0);
	assert(test.array_view() == { 0, 2, 3, 0 });
	assert(test.index_of(0)!! == 0);
	assert(test.rindex_of(0)!! == 3);
	test.remove_item(0);
	assert(test.len() == 2);
	assert(test.array_view() == { 2, 3 });
}

fn void compact()
{
	PtrList test;
	test.add_array({ null, &test });
	assert(test.compact_count() == 1);
	test.push(null);
	assert(test.compact_count() == 1);
	assert(test.len() == 3);
	assert(test.compact() == 2);
	assert(test.len() == 1);
	assert(test.compact() == 0);
}

fn void reverse()
{
	IntList test;

	test.reverse();
	test.add_array({ 1, 2 });
	test.push(3);
	assert(test.array_view() == { 1, 2, 3});
	test.reverse();
	assert(test.array_view() == { 3, 2, 1 });
	test.push(10);
	assert(test.array_view() == { 3, 2, 1, 10 });
	test.reverse();
	assert(test.array_view() == { 10, 1, 2, 3 });
}

fn void remove_if()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&filter);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&select);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});
}

fn void init_with_array()
{
	IntList foo;
	foo.init_with_array(mem, { 1, 2, 3});
	defer foo.free();
	assert(foo.len() == 3);
	assert(foo[2] == 3);
}

fn void init_with_temp_array()
{
	IntList foo;
	foo.tinit_with_array({ 1, 2, 3});
	assert(foo.len() == 3);
	assert(foo[2] == 3);
}

fn void remove_using_test()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i < *(int*)ctx, &&10);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});
}

fn void retain_if()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&select);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&filter);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});
}

fn void retain_using_test()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i < *(int*)ctx, &&10);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});
}

fn void is_initialized()
{
	IntList test;
	
	assert(!test.is_initialized());
	test.init(mem);
	assert(test.is_initialized());
	test.free();
}

module list_test;

fn bool filter(int* i)
{
	return *i >= 10;
}

fn bool select(int* i)
{
	return *i < 10;
}