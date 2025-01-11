module elastic_array_test @test;
import std::collections::elastic_array;

def IntList = ElasticArray(<int, 10>);
def PtrList = ElasticArray(<void*, 10>);

fn void delete_contains_index()
{
	IntList test;
	test.add_array({ 1, 2 });
	assert(test.contains(1));
	assert(test.contains(2));
	assert(!test.contains(0));
	assert(!test.contains(3));
	assert(test.array_view() == int[]{ 1, 2 });
	test.push(3);
	assert(test.array_view() == int[]{ 1, 2, 3 });
	assert(test.contains(3));
	test[0] = 10;
	assert(test.contains(10));
	test.remove_item(10);
	assert(test.array_view() == int[]{ 2, 3 });
	assert(!test.contains(1));
	assert(test.contains(2));
	assert(test.len() == 2);
	test.push(0);
	test.insert_at(0, 0);
	assert(test.array_view() == int[]{ 0, 2, 3, 0 });
	assert(test.index_of(0)!! == 0);
	assert(test.rindex_of(0)!! == 3);
	test.remove_item(0);
	assert(test.len() == 2);
	assert(test.array_view() == int[]{ 2, 3 });
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
	assert(test.array_view() == int[] { 1, 2, 3});
	test.reverse();
	assert(test.array_view() == int[] { 3, 2, 1 });
	test.push(10);
	assert(test.array_view() == int[] { 3, 2, 1, 10 });
	test.reverse();
	assert(test.array_view() == int[] { 10, 1, 2, 3 });
}

fn void remove_if()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&filter);
	assert(removed == 3);
	assert(test.array_view() == int[]{1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_if(&select);
	assert(removed == 2);
	assert(test.array_view() == int[]{11, 10, 20});
}


fn void remove_using_test()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == int[]{1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i < *(int*)ctx, &&10);
	assert(removed == 2);
	assert(test.array_view() == int[]{11, 10, 20});
}

fn void retain_if()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&select);
	assert(removed == 3);
	assert(test.array_view() == int[]{1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.retain_if(&filter);
	assert(removed == 2);
	assert(test.array_view() == int[]{11, 10, 20});
}

fn void retain_using_test()
{
	IntList test;
	usz removed;

	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i >= *(int*)ctx, &&10);
	assert(removed == 3);
	assert(test.array_view() == int[]{1, 2});

	test.clear();
	test.add_array({ 1, 11, 2, 10, 20 });
	removed = test.remove_using_test(fn bool(i, ctx) => *i < *(int*)ctx, &&10);
	assert(removed == 2);
	assert(test.array_view() == int[]{11, 10, 20});
}

module elastic_array_test;

fn bool filter(int* i)
{
	return *i >= 10;
}

fn bool select(int* i)
{
	return *i < 10;
}