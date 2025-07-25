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

	IntSet union_set = set1.tmerge(&set2);
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
	defer diff_set.free();
	assert(diff_set.contains(1));
	assert(!diff_set.contains(2));
	assert(!diff_set.contains(3));
	assert(!diff_set.contains(4));
	assert(diff_set.len() == 1);

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

fn void is_initialized_test()
{
	IntSet test;
	assert(!test.is_initialized());
	test.tinit();
	assert(test.is_initialized());
	test.free();
}
