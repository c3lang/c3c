module tree_test @test;
import std::collections::set;

fn void basic_operations()
{
	TreeSet{int} tree;
	defer tree.free();
	tree.@each(;i) {};
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

fn void sorted_iteration_order()
{
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	int[] values = { 50, 10, 90, 30, 70, 20, 80, 40, 60, -5, 0 };
	foreach (value : values)
	{
		tree.add(value);
	}

	int[] expected = { -5, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90 };

	// @each should visit every element ...
	int each_count = 0;
	tree.@each(; int value)
	{
		each_count++;
	};
	assert(each_count == expected.len);

	// ... and the manual iterator must yield them in strictly increasing
	// order, matching an in-order traversal of a valid BST.
	TreeSetIterator{int} iter = tree.iter();
	int prev                  = 0;
	bool has_prev             = false;
	int iter_count            = 0;
	while (iter.next())
	{
		int current = *iter.get()!!;
		if (has_prev)
		{
			assert(current > prev);
		}
		assert(current == expected[iter_count]);
		prev = current;
		has_prev = true;
		iter_count++;
	}
	assert(iter_count == expected.len);
}

fn void remove_nonexistent_faults_consistently()
{
	// An empty tree should fault the exact same way a populated tree
	// does when asked to remove a value that isn't present. (The
	// original edge_cases() test only checked "doesn't crash" on the
	// empty case and never asserted a fault there, which masked this.)
	TreeSet{int} empty;
	empty.tinit();
	defer empty.free();
	assert(@catch(empty.remove(0)));
	assert(empty.is_empty());

	TreeSet{int} single;
	single.tinit();
	defer single.free();
	single.add(5);
	assert(@catch(single.remove(6)));
	assert(single.len() == 1);
	assert(single.contains(5));

	// Removing the same value twice: the second removal must fault.
	single.remove(5);
	assert(single.is_empty());
	assert(@catch(single.remove(5)));

	TreeSet{int} many;
	many.tinit_with_values(1, 2, 3, 4, 5);
	defer many.free();
	assert(@catch(many.remove(100)));
	assert(@catch(many.remove(-100)));
	assert(many.len() == 5);
	for (int i = 1; i <= 5; i++)
	{
		assert(many.contains(i));
	}
}

fn void remove_min_and_max_repeatedly()
{
	// Repeatedly stripping the smallest and largest remaining values
	// forces deletion of the root and of nodes with only one child,
	// which is where naive RB-tree delete/rebalance logic tends to
	// break.
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	for (int i = 0; i < 50; i++)
	{
		tree.add(i);
	}

	int lo = 0;
	int hi = 49;
	while (lo <= hi)
	{
		assert(tree.contains(lo));
		tree.remove(lo);
		assert(!tree.contains(lo));
		lo++;

		if (lo > hi) break;

		assert(tree.contains(hi));
		tree.remove(hi);
		assert(!tree.contains(hi));
		hi--;
	}

	assert(tree.is_empty());
	assert(tree.len() == 0);
}

fn void remove_all_ascending_order()
{
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	int n = 200;
	for (int i = 0; i < n; i++)
	{
		tree.add(i);
	}
	assert(tree.len() == n);

	for (int i = 0; i < n; i++)
	{
		assert(tree.contains(i));
		tree.remove(i);
		assert(!tree.contains(i));
		assert(tree.len() == (sz)n - i - 1);
		// Spot-check that unrelated elements survived this deletion.
		for (int j = i + 1; j < n; j += 37)
		{
			assert(tree.contains(j));
		}
	}
	assert(tree.is_empty());
}

fn void remove_all_descending_order()
{
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	int n = 200;
	for (int i = 0; i < n; i++)
	{
		tree.add(i);
	}
	assert(tree.len() == n);

	for (int i = n - 1; i >= 0; i--)
	{
		assert(tree.contains(i));
		tree.remove(i);
		assert(!tree.contains(i));
		assert(tree.len() == i);
		for (int j = 0; j < i; j += 37)
		{
			assert(tree.contains(j));
		}
	}
	assert(tree.is_empty());
}

fn void duplicate_values_in_bulk_init()
{
	TreeSet{int} tree;
	tree.tinit_with_values(3, 1, 2, 3, 2, 1, 3, 1, 2);
	defer tree.free();

	assert(tree.len() == 3);
	assert(tree.contains(1));
	assert(tree.contains(2));
	assert(tree.contains(3));

	int[] values = { 7, 7, 8, 8, 9 };
	TreeSet{int} tree2;
	tree2.tinit_from_values(values);
	defer tree2.free();

	assert(tree2.len() == 3);
	assert(tree2.contains(7));
	assert(tree2.contains(8));
	assert(tree2.contains(9));
}

fn void deep_copy_independence()
{
	TreeSet{int} original;
	original.tinit_with_values(1, 2, 3);
	defer original.free();

	TreeSet{int} copy;
	copy.tinit_from_tree(&original);
	defer copy.free();

	// Mutating the copy must not affect the original.
	copy.add(4);
	copy.remove(1);
	assert(original.contains(1));
	assert(!original.contains(4));
	assert(original.len() == 3);
	assert(copy.len() == 3);
	assert(copy.contains(2));
	assert(copy.contains(3));
	assert(copy.contains(4));
	assert(!copy.contains(1));

	// Mutating the original after the copy was taken must not leak
	// into the copy either (i.e. this should be a deep, not shallow,
	// copy of the tree structure).
	original.add(99);
	assert(!copy.contains(99));
}

fn void extreme_int_values()
{
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	assert(tree.add(int::min));
	assert(tree.add(int::max));
	assert(tree.add(0));
	assert(tree.add(-1));
	assert(tree.add(1));

	assert(tree.len() == 5);
	assert(tree.contains(int::min));
	assert(tree.contains(int::max));

	int[5] expected           = { int::min, -1, 0, 1, int::max };
	TreeSetIterator{int} iter = tree.iter();
	int i                     = 0;
	while (iter.next())
	{
		assert(*iter.get()!! == expected[i]);
		i++;
	}
	assert(i == 5);

	tree.remove(int::min);
	assert(!tree.contains(int::min));
	tree.remove(int::max);
	assert(!tree.contains(int::max));
	assert(tree.len() == 3);
}

fn void reuse_after_clear()
{
	TreeSet{int} tree;
	tree.tinit_with_values(1, 2, 3, 4, 5);
	defer tree.free();

	tree.clear();
	assert(tree.is_empty());

	for (int i = 100; i < 150; i++)
	{
		assert(tree.add(i));
	}
	assert(tree.len() == 50);
	for (int i = 100; i < 150; i++)
	{
		assert(tree.contains(i));
	}
	assert(!tree.contains(1));

	// Repeated clear/reuse cycles shouldn't leak state between them.
	for (int cycle = 0; cycle < 3; cycle++)
	{
		tree.clear();
		assert(tree.is_empty());
		tree.add(cycle);
		assert(tree.len() == 1);
		assert(tree.contains(cycle));
	}
}

fn void add_remove_add_cycle()
{
	// Re-inserting values that were previously removed must behave
	// exactly like inserting them fresh.
	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	for (int round = 0; round < 5; round++)
	{
		for (int i = 0; i < 20; i++)
		{
			assert(tree.add(i));
		}
		assert(tree.len() == 20);
		for (int i = 0; i < 20; i++)
		{
			tree.remove(i);
		}
		assert(tree.is_empty());
	}
}

fn void model_based_random_stress()
{
	// Property-based check: drive the tree with a long, deterministic
	// pseudo-random sequence of add/remove operations while mirroring
	// expected membership in a plain bool array (the "model"). After
	// every single operation the tree's reported length must match the
	// model, and at the end contains(), the count, and iteration order
	// must all agree with the model too. This exercises every shape of
	// rotation/rebalance far more thoroughly than hand-picked cases can.
	const int RANGE = 256;
	const int OPS   = 3000;

	TreeSet{int} tree;
	tree.tinit();
	defer tree.free();

	bool[RANGE] present;
	int expected_len = 0;


	var rand = fn uint ()
	{
		// xorshift32 - deterministic, no external RNG dependency.
		static uint seed = 2463534242; // arbitrary non-zero xorshift32 seed
		seed ^= seed << 13;
		seed ^= seed >> 17;
		seed ^= seed << 5;
		return seed;
	};

	for (int op = 0; op < OPS; op++)
	{
		uint seed   = rand();
		int value   = (int)(seed % (uint)RANGE);
		bool do_add = (seed & 0x100) == 0;

		if (do_add)
		{
			bool added = tree.add(value);
			if (present[value])
			{
				assert(!added);
			}
			else
			{
				assert(added);
				present[value] = true;
				expected_len++;
			}
		}
		else
		{
			if (present[value])
			{
				tree.remove(value);
				present[value] = false;
				expected_len--;
			}
			else
			{
				assert(@catch(tree.remove(value)));
			}
		}

		assert(tree.len() == expected_len);
	}

	for (int v = 0; v < RANGE; v++)
	{
		assert(tree.contains(v) == present[v]);
	}

	int prev                  = 0;
	bool has_prev             = false;
	int count                 = 0;
	TreeSetIterator{int} iter = tree.iter();
	while (iter.next())
	{
		int current = *iter.get()!!;
		if (has_prev)
		{
			assert(current > prev);
		}
		prev = current;
		has_prev = true;
		count++;
	}
	assert(count == expected_len);
}
