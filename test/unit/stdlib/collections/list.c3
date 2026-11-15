module list_test @test;
import std::collections::list;

alias IntList = List{int};
alias PtrList = List{void*};

struct Overalign
{
	float[<4>] x @align(128);
}
alias Vec3 = int[<3>];
fn void veclist()
{
	List{Vec3} values;
    List{Vec3} new_vertices;
	values.init(mem, 200);
	new_vertices.init(mem);
	defer new_vertices.free();
	defer values.free();
	Vec3 x = {1,2,3};
	for (int i = 0; i < 20; i++) new_vertices.push(x);
    values.clear();
    values.add_all(&new_vertices);
    Vec3* entries = new_vertices.entries;
    foreach (Vec3 entry : new_vertices)
    {
        assert(entry == x);
    }
    foreach (Vec3 entry : values)
    {
        assert(entry == x);
    }
}

alias Vec4 = int[<4>] @simd;
fn void veclist_simd()
{
	List{Vec4} values;
    List{Vec4} new_vertices;
	values.init(mem, 200);
	new_vertices.init(mem);
	defer new_vertices.free();
	defer values.free();
	Vec4 x = {1,2,3,5};
	for (int i = 0; i < 20; i++) new_vertices.push(x);
    values.clear();
    values.add_all(&new_vertices);
    Vec4* entries = new_vertices.entries;
    foreach (Vec4 entry : new_vertices)
    {
        assert(entry == x);
    }
    foreach (Vec4 entry : values)
    {
        assert(entry == x);
    }
}

alias OveralignList = List{Overalign};
fn void overaligned_type()
{
	OveralignList l;
	defer l.free();

	Overalign y;
	for (int i = 0; i < 1000; i++) l.push(y);
	assert((sz)l.get_ref(2) - (sz)l.get_ref(1) == Overalign::size);
}

fn void remove_at()
{
	IntList test;
	test.init(mem);
	defer test.free();
	test.push_all({ 1, 2, 3, 4 });
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

fn void remove_unordered_at()
{
	IntList test;
	test.init(mem);
	defer test.free();
	test.push_all({ 1, 2, 3, 4 });
	test::eq(test.array_view(), (int[]){ 1, 2, 3, 4 });
	test.remove_unordered_at(0);
	test::eq(test.array_view(), (int[]){ 4, 2, 3 });
	test.remove_unordered_at(1);
	test::eq(test.array_view(), (int[]){ 4, 3 });
	test.remove_unordered_at(1);
	test::eq(test.array_view(), (int[]){ 4 });
	test.remove_unordered_at(0);
	test::eq(test.array_view(), (int[]){ });
}

fn void delete_contains_index()
{
	IntList test;

	test.push_all({ 1, 2 });
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
	test.push_all({ null, &test });
	assert(test.compact_count() == 1);
	test.push(null);
	assert(test.compact_count() == 1);
	assert(test.len() == 3);
	assert(test.compact() == 2);
	assert(test.len() == 1);
	assert(test.compact() == 0);
}

fn void compact_runs()
{
	PtrList test;
	int a;
	int b;
	int c;
	test.push_all({ null, null, &a, null, null, &b, &c, null });
	assert(test.compact() == 5);
	assert(test.len() == 3);
	assert(test[0] == &a);
	assert(test[1] == &b);
	assert(test[2] == &c);
	assert(test.compact() == 0);
	test.clear();
	test.push_all({ null, null, null });
	assert(test.compact() == 3);
	assert(test.len() == 0);
}

fn void remove_item_many()
{
	IntList test;
	test.push_all({ 1, 2, 2, 3, 2, 2, 2, 4, 2 });
	assert(test.remove_item(2) == 6);
	assert(test.array_view() == { 1, 3, 4 });
	assert(test.remove_item(2) == 0);
	assert(test.array_view() == { 1, 3, 4 });
	test.clear();
	test.push_all({ 5, 5, 5 });
	assert(test.remove_item(5) == 3);
	assert(test.len() == 0);
	assert(test.remove_item(5) == 0);
	test.push_all({ 7, 5, 5, 8, 5 });
	assert(test.remove_item(5) == 3);
	assert(test.array_view() == { 7, 8 });
}

fn void insert_at_positions()
{
	IntList test;
	test.insert_at(0, 3);
	test.insert_at(0, 1);
	test.insert_at(1, 2);
	test.insert_at(3, 4);
	assert(test.array_view() == { 1, 2, 3, 4 });

	// Insert in the middle across several capacity growths, compare with a plain array.
	test.clear();
	int[300] expected;
	for (int i = 0; i < 300; i++)
	{
		sz index = test.len() / 2;
		test.insert_at(index, i);
		for (sz j = i; j > index; j--) expected[j] = expected[j - 1];
		expected[index] = i;
	}
	assert(test.len() == 300);
	for (sz i = 0; i < 300; i++) assert(test[i] == expected[i]);
}

fn void insert_at_overaligned()
{
	List{Overalign} test;
	test.push_front({ { 3, 3, 3, 3 } });
	test.push_front({ { 1, 1, 1, 1 } });
	test.insert_at(1, { { 2, 2, 2, 2 } });
	test.insert_at(3, { { 4, 4, 4, 4 } });
	assert(test.len() == 4);
	for (sz i = 0; i < 4; i++) assert(test[i].x[0] == (float)(i + 1));
}

fn void push_growth()
{
	IntList test;
	for (int i = 0; i < 1000; i++) test.push(i);
	assert(test.len() == 1000);
	for (int i = 0; i < 1000; i++) assert(test[i] == i);
}

fn void reverse()
{
	IntList test;

	test.reverse();
	test.push_all({ 1, 2 });
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
	sz removed;

	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&filter);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&select);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});

	// Verify each element is evaluated strictly once
	call_count = 0;
	test.clear();
	test.push_all({ 1, 2, 3, 4, 5, 6, 7 });
	removed = test.remove_if(&counting_filter);
	assert(call_count == 7);
	assert(removed == 4);
	assert(test.array_view() == { 2, 4, 6 });

	// Remove none
	test.clear();
	test.push_all({ 2, 4, 6 });
	removed = test.remove_if(&counting_filter);
	assert(removed == 0);
	assert(test.array_view() == { 2, 4, 6 });

	// Remove all
	test.clear();
	test.push_all({ 1, 3, 5 });
	removed = test.remove_if(&counting_filter);
	assert(removed == 3);
	assert(test.len() == 0);

	// Empty list
	test.clear();
	removed = test.remove_if(&counting_filter);
	assert(removed == 0);
	assert(test.len() == 0);
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
	sz removed;

	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i < *(int*)ctx, &&10);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});
}

fn void retain_if()
{
	IntList test;
	sz removed;

	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&select);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&filter);
	assert(removed == 2);
	assert(test.array_view() == {11, 10, 20});

	// Verify each element is evaluated strictly once
	call_count = 0;
	test.clear();
	test.push_all({ 1, 2, 3, 4, 5, 6, 7 });
	removed = test.retain_if(&counting_filter);
	assert(call_count == 7);
	assert(removed == 3);
	assert(test.array_view() == { 1, 3, 5, 7 });

	// Retain all
	test.clear();
	test.push_all({ 1, 3, 5 });
	removed = test.retain_if(&counting_filter);
	assert(removed == 0);
	assert(test.array_view() == { 1, 3, 5 });

	// Retain none
	test.clear();
	test.push_all({ 2, 4, 6 });
	removed = test.retain_if(&counting_filter);
	assert(removed == 3);
	assert(test.len() == 0);

	// Empty list
	test.clear();
	removed = test.retain_if(&counting_filter);
	assert(removed == 0);
	assert(test.len() == 0);
}

fn void retain_using_test()
{
	IntList test;
	sz removed;

	test.push_all({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == {1, 2});

	test.clear();
	test.push_all({ 1, 11, 2, 10, 20 });
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

int call_count;

fn bool counting_filter(int* i)
{
	call_count++;
	return (*i % 2) != 0;
}