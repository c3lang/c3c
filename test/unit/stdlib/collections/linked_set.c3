module linked_set_test @test;
import std::collections::set;

alias TestLinkedHashSet = LinkedHashSet{String};

fn void linked_set_basic()
{
	TestLinkedHashSet set;
	assert(!set.is_initialized());
	set.tinit();
	assert(set.is_initialized());
	assert(set.is_empty());
	assert(set.len() == 0);

	assert(set.add("a"));
	assert(!set.is_empty());
	assert(set.len() == 1);
	set.remove("a");
	assert(set.is_empty());

	String[] values = { "key1", "key2", "key3" };
	foreach (value : values)
	{
		assert(set.add(value));
	}
	assert(set.len() == values.len);
	foreach (value : values)
	{
		assert(set.contains(value));
	}
}

fn void linked_set_insertion_order()
{
	TestLinkedHashSet set;
	set.tinit();
	
	String[] values = { "first", "second", "third", "fourth" };
	foreach (value : values)
	{
		set.add(value);
	}
	
	// Test iteration follows insertion order
	usz index = 0;
	set.@each(; String value)
	{
		assert(value == values[index]);
		index++;
	};
	
	// Test that removing and re-inserting changes order
	set.remove("second");
	set.add("second");
	
	String[] new_order = { "first", "third", "fourth", "second" };
	index = 0;
	set.@each(; String value)
	{
		assert(value == new_order[index]);
		index++;
	};
}

fn void linked_set_init_with_values()
{
	TestLinkedHashSet set;
	set.tinit_with_values("a", "b", "c");
	
	assert(set.len() == 3);
	assert(set.contains("a"));
	assert(set.contains("b"));
	assert(set.contains("c"));
	
	// Verify order
	String[] expected_order = { "a", "b", "c" };
	usz index = 0;
	set.@each(; String value)
	{
		assert(value == expected_order[index]);
		index++;
	};
}

fn void linked_set_remove()
{
	TestLinkedHashSet set;
	assert(!@ok(set.remove("A")));
	set.tinit();
	assert(!@ok(set.remove("A")));
	set.add("A");
	assert(@ok(set.remove("A")));
	
	set.add("a");
	set.add("b");
	set.add("c");
	set.remove("b");
	
	String[] expected_order = { "a", "c" };
	usz index = 0;
	set.@each(; String value)
	{
		assert(value == expected_order[index]);
		index++;
	};
}

fn void linked_set_copy()
{
	TestLinkedHashSet original;
	original.tinit();

	original.add("aa");
	original.add("b");
	original.add("bb");

	TestLinkedHashSet copy;
	copy.tinit_from_set(&original);

	assert(copy.len() == original.len());
	
	String[] expected_order = { "aa", "b", "bb" };
	usz index = 0;
	copy.@each(; String value)
	{
		assert(value == expected_order[index]);
		index++;
	};
}

fn void linked_set_iterators()
{
	TestLinkedHashSet set;
	set.tinit_with_values("a", "b", "c");
	
	// Test entry iterator
	usz count = 0;
	LinkedHashSetIterator{String} iter = set.iter();
	while (iter.next())
	{
		count++;
		assert(iter.get()!!.len > 0);
	}
	assert(count == 3);
	
	// Test direct each macro
	count = 0;
	set.@each(; String value) {
		count++;
		assert(value.len > 0);
	};
	assert(count == 3);
}

fn void linked_set_clear()
{
	TestLinkedHashSet set;
	set.tinit_with_values("a", "b", "c");
	
	assert(set.len() == 3);
	set.clear();
	assert(set.len() == 0);
	assert(set.is_empty());
	
	// Should be able to reuse after clear
	set.add("x");
	assert(set.len() == 1);
	assert(set.contains("x"));
}

fn void linked_set_operations()
{
	TestLinkedHashSet set1;
	set1.tinit_with_values("a", "b", "c");
	
	TestLinkedHashSet set2;
	set2.tinit_with_values("b", "c", "d");
	
	// Test union
	TestLinkedHashSet union_set = set1.tset_union(&set2);
	defer union_set.free();
	assert(union_set.contains("a"));
	assert(union_set.contains("b"));
	assert(union_set.contains("c"));
	assert(union_set.contains("d"));
	assert(union_set.len() == 4);
	
	// Verify union preserves order (elements from first set first)
	String[] expected_union_order = { "a", "b", "c", "d" };
	usz index = 0;
	union_set.@each(; String value) {
		assert(value == expected_union_order[index]);
		index++;
	};
	
	// Test intersection
	TestLinkedHashSet intersect_set = set1.tintersection(&set2);
	defer intersect_set.free();
	assert(intersect_set.contains("b"));
	assert(intersect_set.contains("c"));
	assert(!intersect_set.contains("a"));
	assert(!intersect_set.contains("d"));
	assert(intersect_set.len() == 2);
	
	// Test difference
	TestLinkedHashSet diff_set = set1.tdifference(&set2);
	defer diff_set.free();
	assert(diff_set.contains("a"));
	assert(!diff_set.contains("b"));
	assert(!diff_set.contains("c"));
	assert(!diff_set.contains("d"));
	assert(diff_set.len() == 1);
	
	// Test subset
	TestLinkedHashSet subset;
	subset.tinit_with_values("b", "c");
	assert(subset.is_subset(&set1));
	assert(!set1.is_subset(&subset));
}

alias IntLinkedSet = LinkedHashSet{int};

fn void linked_set_edge_cases()
{
	// Test empty set
	IntLinkedSet empty;
	empty.tinit();
	defer empty.free();
	
	assert(empty.is_empty());
	assert(!empty.contains(0));
	empty.remove(0); // Shouldn't crash
	
	// Test large set
	IntLinkedSet large;
	large.tinit();
	defer large.free();
	
	// Insert in reverse order to test ordering
	for (int i = 1000; i > 0; i--) {
		large.add(i);
	}
	assert(large.len() == 1000);
	
	// Verify order is maintained
	int expected = 1000;
	large.@each(; int value) {
		assert(value == expected);
		expected--;
	};
	
	// Test clear
	large.clear();
	assert(large.is_empty());
	for (int i = 1; i <= 1000; i++) {
		assert(!large.contains(i));
	}
}

fn void linked_set_string_values()
{
	TestLinkedHashSet set;
	set.tinit();
	defer set.free();
	
	assert(set.add("hello"));
	assert(set.add("world"));
	assert(!set.add("hello"));
	
	assert(set.contains("hello"));
	assert(set.contains("world"));
	assert(!set.contains("foo"));
	
	// Test order
	String[] expected_order = { "hello", "world" };
	usz index = 0;
	set.@each(; String value) {
		assert(value == expected_order[index]);
		index++;
	};
	
	set.remove("hello");
	assert(!set.contains("hello"));
	assert(set.len() == 1);
	
	// Test order after removal
	assert(set.contains("world"));
	set.@each(; String value) {
		assert(value == "world");
	};
}

fn void linked_set_is_initialized()
{
	TestLinkedHashSet test;
	assert(!test.is_initialized());
	test.tinit();
	assert(test.is_initialized());
	test.free();
}
