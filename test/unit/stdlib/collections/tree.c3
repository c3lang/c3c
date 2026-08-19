module tree_test @test;
import std::collections::set;

fn void basic_operations()
{
	TreeSet{int} tree;
	defer tree.free();

	assert(tree.is_empty());
	assert(!tree.contains(1));

	assert(tree.add(1));
	assert(tree.contains(1));
	assert(!tree.is_empty());
	assert(tree.len() == 1);

	assert(!tree.add(1));
	assert(tree.len() == 1);

	assert(tree.add(2));
	assert(tree.add(3));
	assert(tree.len() == 3);
	assert(tree.contains(2));
	assert(tree.contains(3));

	tree.remove(2);
	assert(!tree.contains(2));
	assert(tree.len() == 2);

	tree.clear();
	assert(tree.is_empty());
	assert(!tree.contains(1));
}

fn void initialization_methods()
{
	TreeSet{int} tree1;
	tree1.tinit();
	defer tree1.free();
	assert(tree1.is_initialized());

	TreeSet{int} tree2;
	tree2.tinit_with_values(1, 2, 3);
	defer tree2.free();
	assert(tree2.contains(1));
	assert(tree2.contains(2));
	assert(tree2.contains(3));
	assert(tree2.len() == 3);

	int[] values = { 4, 5, 6 };
	TreeSet{int} tree3;
	tree3.tinit_from_values(values);
	defer tree3.free();
	assert(tree3.contains(4));
	assert(tree3.contains(5));
	assert(tree3.contains(6));
	assert(tree3.len() == 3);

	TreeSet{int} tree4;
	tree4.tinit_from_tree(&tree3);
	defer tree4.free();
	assert(tree4.contains(4));
	assert(tree4.contains(5));
	assert(tree4.contains(6));
	assert(tree4.len() == 3);
}

fn void iterator_test()
{
	TreeSet{int} tree;
	tree.tinit_with_values(1, 2, 3);
	defer tree.free();

	int count   = 0;
	bool found1 = false;
	bool found2 = false;
	bool found3 = false;

	tree.@each(; int value)
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

	TreeSetIterator{int} iter = tree.iter();
	count = 0;
	int sum;
	while (iter.next())
	{
		count++;
		sum += *iter.get()!!;
	}
	assert(count == 3);
	assert(sum == 6);
}

fn void edge_cases()
{
	TreeSet{int} empty;
	empty.tinit();
	defer empty.free();

	assert(empty.is_empty());
	assert(!empty.contains(0));
	empty.remove(0); // Shouldn't crash

	TreeSet{int} large;
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

fn void is_initialized_test()
{
	TreeSet{int} test;
	assert(!test.is_initialized());
	test.tinit();
	assert(test.is_initialized());
	test.free();
}
