module listtests @test;
import std::collections::list;

def IntList = List<int>;
def PtrList = List<void*>;

fn void! test_delete_contains_index()
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
	test.remove(1);
	assert(test.array_view() == int[]{ 2, 3 });
	assert(!test.contains(1));
	assert(test.contains(2));
	assert(test.len() == 2);
	test.push(0);
	test.insert_at(0, 0);
	assert(test.array_view() == int[]{ 0, 2, 3, 0 });
	assert(test.index_of(0)! == 0);
	assert(test.rindex_of(0)! == 3);
	test.remove(0);
	assert(test.len() == 2);
	assert(test.array_view() == int[]{ 2, 3 });
}

fn void! test_compact()
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

fn void! test_reverse()
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