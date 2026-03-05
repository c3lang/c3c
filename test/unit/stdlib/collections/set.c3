module set_test @test;
import std::collections::set;

alias IntSet = HashSet{int};

fn void basic_operations()
{
	IntSet set;
	defer set.free();

	assert(set.is_empty());
	assert(!set.contains(1));

	assert(set.add(1));
	assert(set.contains(1));
	assert(!set.is_empty());
	assert(set.len() == 1);

	assert(!set.add(1));
	assert(set.len() == 1);

	assert(set.add(2));
	assert(set.add(3));
	assert(set.len() == 3);
	assert(set.contains(2));
	assert(set.contains(3));

	set.remove(2);
	assert(!set.contains(2));
	assert(set.len() == 2);

	set.clear();
	assert(set.is_empty());
	assert(!set.contains(1));
}

fn void initialization_methods()
{
	IntSet set1;
	set1.tinit();
	defer set1.free();
	assert(set1.is_initialized());

	IntSet set2;
	set2.tinit_with_values(1, 2, 3);
	defer set2.free();
	assert(set2.contains(1));
	assert(set2.contains(2));
	assert(set2.contains(3));
	assert(set2.len() == 3);

	int[] values = {4, 5, 6};
	IntSet set3;
	set3.tinit_from_values(values);
	defer set3.free();
	assert(set3.contains(4));
	assert(set3.contains(5));
	assert(set3.contains(6));
	assert(set3.len() == 3);

	IntSet set4;
	set4.tinit_from_set(&set3);
	defer set4.free();
	assert(set4.contains(4));
	assert(set4.contains(5));
	assert(set4.contains(6));
	assert(set4.len() == 3);
}

fn void set_operations()
{
	IntSet set1;
	set1.tinit_with_values(1, 2, 3);
	defer set1.free();

	IntSet set2;
	set2.tinit_with_values(2, 3, 4);
	defer set2.free();

	IntSet union_set = set1.tset_union(&set2);
	defer union_set.free();
	assert(union_set.contains(1));
	assert(union_set.contains(2));
	assert(union_set.contains(3));
	assert(union_set.contains(4));
	assert(union_set.len() == 4);

	IntSet intersect_set = set1.tintersection(&set2);
	defer intersect_set.free();
	assert(intersect_set.contains(2));
	assert(intersect_set.contains(3));
	assert(!intersect_set.contains(1));
	assert(!intersect_set.contains(4));
	assert(intersect_set.len() == 2);

	IntSet diff_set = set1.tdifference(&set2);
	assert(diff_set.contains(1));
	assert(!diff_set.contains(2));
	assert(!diff_set.contains(3));
	assert(!diff_set.contains(4));
	assert(diff_set.len() == 1);

	IntSet sdiff_set = set1.tsymmetric_difference(&set2);
	assert(sdiff_set.contains(1));
	assert(!sdiff_set.contains(2));
	assert(!sdiff_set.contains(3));
	assert(sdiff_set.contains(4));
	assert(sdiff_set.len() == 2);

	IntSet subset;
	subset.tinit_with_values(2, 3);
	defer subset.free();
	assert(subset.is_subset(&set1));
	assert(!set1.is_subset(&subset));
}

fn void iterator_test()
{
	IntSet set;
	set.tinit_with_values(1, 2, 3);
	defer set.free();

	int count = 0;
	bool found1 = false; bool found2 = false; bool found3 = false;

	set.@each(; int value)
	{
		count++;
		switch (value)
		{
			case 1: found1 = true;
			case 2: found2 = true;
			case 3: found3 = true;
		}
	};

	assert(count == 3);
	assert(found1 && found2 && found3);

	HashSetIterator {int} iter = set.iter();
	count = 0;
	while (@ok(iter.next()))
	{
		count++;
	}
	assert(count == 3);
}

fn void edge_cases()
{
	IntSet empty;
	empty.tinit();
	defer empty.free();

	assert(empty.is_empty());
	assert(!empty.contains(0));
	empty.remove(0); // Shouldn't crash

	IntSet large;
	large.tinit();
	defer large.free();

	for (int i = 0; i < 1000; i++)
	{
		large.add(i);
	}
	assert(large.len() == 1000);
	for (int i = 0; i < 1000; i++)
	{
		assert(large.contains(i));
	}

	assert(@catch(large.remove(1001)));
	assert(large.len() == 1000);

	large.clear();
	assert(large.is_empty());
	for (int i = 0; i < 1000; i++)
	{
		assert(!large.contains(i));
	}
}

alias StringSet = HashSet{String};

fn void string_set_test()
{
	StringSet set;
	set.tinit();
	defer set.free();

	assert(set.add("hello"));
	assert(set.add("world"));
	assert(!set.add("hello"));

	assert(set.contains("hello"));
	assert(set.contains("world"));
	assert(!set.contains("foo"));

	set.remove("hello");
	assert(!set.contains("hello"));
	assert(set.len() == 1);
}

fn void add_all_test()
{
	StringSet set1;
	set1.init(mem);
	defer set1.free();

	String[] list = {"hello", "world", "hello"};
	usz total = set1.add_all(list);
	assert(total == 2);

	assert(set1.contains("hello"));
	assert(set1.contains("world"));
	assert(!set1.contains("foo"));

	set1.remove("hello");
	assert(!set1.contains("hello"));
	assert(set1.len() == 1);

	StringSet set2;
	set2.init(mem);
	defer set2.free();

	total = set2.add_all({"foo", "bar"});
	assert(total == 2);

	total = set2.add_all_from(&set1);
	assert(total == 1);

	assert(set2.contains("foo"));
	assert(set2.contains("bar"));
	assert(set2.contains("world"));
	assert(!set2.contains("hello"));
}

fn void remove_all_test()
{
	String[] list1 = {"foo", "bar", "baz"};
	String[] list2 = {"Hello", "World", "foo"};
	String[] list3 = {"the", "C3", "compiler", "said", "Hello"};

	StringSet set1;
	set1.init_from_values(mem, list1);
	defer set1.free();
	assert(set1.len() == 3);

	StringSet set2;
	set2.init_from_values(mem, list1);
	defer set2.free();
	assert(set1.len() == 3);

	usz total = set2.add_all(list2);
	assert(total == 2);
	assert(set2.len() == 5);

	total = set2.remove_all(list1);
	assert(total == 3);

	assert(set2.len() == 2);
	assert(set2.contains("Hello"));
	assert(set2.contains("World"));
	assert(!set2.contains("foo"));
	assert(!set2.contains("bar"));
	assert(!set2.contains("baz"));

	total = set1.add_all(list3);
	assert(total == 5);
	assert(set1.len() == 8);

	total = set1.remove_all_from(&set2);
	assert(total == 1);

	assert(set1.len() == 7);
	assert(set1.contains("foo"));
	assert(set1.contains("bar"));
	assert(set1.contains("baz"));
	assert(set1.contains("the"));
	assert(set1.contains("C3"));
	assert(set1.contains("compiler"));
	assert(set1.contains("said"));
	assert(!set1.contains("Hello"));
}

fn void values_test()
{
	StringSet set;
	set.init(mem);
	defer set.free();

	String[] values = set.values(mem);
	assert(values.len == 0);
	free(values);

	String[] list1 = {"foo", "bar", "baz"};
	set.add_all(list1);
	assert(set.len() == 3);

	values = set.values(mem);
	assert(values.len == 3);
	assert(array::contains(values, "foo"));
	assert(array::contains(values, "bar"));
	assert(array::contains(values, "baz"));
	free(values);

	set.remove("bar");
	values = set.tvalues();
	assert(values.len == 2);
	assert(array::contains(values, "foo"));
	assert(array::contains(values, "baz"));
	assert(!array::contains(values, "bar"));
}

fn void is_initialized_test()
{
	IntSet test;
	assert(!test.is_initialized());
	test.tinit();
	assert(test.is_initialized());
	test.free();
}
