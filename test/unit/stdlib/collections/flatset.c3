module flatset_test @test;
import std::collections::flatset;

fn void test_init_tinit()
{
	FlatSet{sz} set;
	set.init(tmem, 32);
	set.add(1);
	set.add(2);
	test::eq(set.contains(1), true);
	test::eq(set.contains(2), true);

	FlatSet{sz} tset;
	tset.tinit();
	tset.add(3);
	test::eq(tset.contains(3), true);
	test::eq(tset.contains(4), false);

	FlatSet{sz} hset;
	hset.init(tmem, 16, 0.5f);
	hset.add(5);
	test::eq(hset.contains(5), true);
}

fn void test_init_from_values()
{
	FlatSet{sz} set;
	set.init_from_values(tmem, { 1, 2, 2, 3, 3, 3 });
	test::eq(set.contains(1), true);
	test::eq(set.contains(2), true);
	test::eq(set.contains(3), true);
	test::eq(set.contains(4), false);
	test::eq(set.tvalues().len, (sz)3);

	FlatSet{sz} tset;
	tset.tinit_from_values({ 5, 5, 6 });
	test::eq(tset.contains(5), true);
	test::eq(tset.contains(6), true);
	test::eq(tset.tvalues().len, (sz)2);
}

fn void test_add_contains()
{
	FlatSet{sz} set;
	set.tinit();

	test::eq(set.add(1), true);
	test::eq(set.add(2), true);
	// Duplicate insert reports false.
	test::eq(set.add(1), false);

	test::eq(set.contains(1), true);
	test::eq(set.contains(2), true);
	test::eq(set.contains(3), false);

	// Enough insertions to trigger grow() several times (default capacity 16).
	for (sz i = 0; i < 100; ++i)
	{
		if (!set.add(i + 10)) return;
	}
	for (sz i = 0; i < 100; ++i)
	{
		test::eq(set.contains(i + 10), true);
	}
	test::eq(set.contains(9999), false);
}

fn void test_remove()
{
	FlatSet{sz} set;
	set.tinit();
	set.add(1);
	set.add(2);

	test::eq(set.remove(1), true);
	test::eq(set.contains(1), false);

	// Removing an absent value reports false.
	test::eq(set.remove(1), false);

	// Re-adding after removal reuses the deleted slot.
	set.add(1);
	test::eq(set.contains(1), true);
	test::eq(set.contains(2), true);
}

fn void test_add_all_from()
{
	FlatSet{sz} a;
	a.tinit();
	a.add(1);
	a.add(2);

	FlatSet{sz} b;
	b.tinit();
	b.add(2);
	b.add(3);

	a.add_all_from(&b);
	test::eq(a.contains(1), true);
	test::eq(a.contains(2), true);
	test::eq(a.contains(3), true);
	test::eq(a.tvalues().len, (sz)3);
}

fn void test_clear()
{
	FlatSet{sz} set;
	set.tinit();
	set.add(1);
	set.add(2);

	set.clear();
	test::eq(set.contains(1), false);
	test::eq(set.contains(2), false);
	test::eq(set.tvalues().len, (sz)0);

	// Set remains usable after clear.
	set.add(3);
	test::eq(set.contains(3), true);
}

fn void test_free()
{
	FlatSet{sz} set;
	set.init(tmem, 32);
	set.add(1);
	test::eq(set.contains(1), true);
	set.free();

	// Set can be re-initialized and reused after free.
	set.init(tmem, 16);
	set.add(2);
	test::eq(set.contains(2), true);
}

fn void test_set_union()
{
	FlatSet{sz} a = range_set(0, 10);
	FlatSet{sz} b = range_set(5, 15);

	FlatSet{sz} res = a.set_union(tmem, &b);
	defer res.free();
	test::eq(res.contains(0), true);
	test::eq(res.contains(5), true);
	test::eq(res.contains(14), true);
	test::eq(res.contains(15), false);
	test::eq(res.tvalues().len, (sz)15);

	FlatSet{sz} tres = a.tset_union(&b);
	defer tres.free();
	test::eq(tres.contains(14), true);
	test::eq(tres.contains(15), false);
}

fn void test_intersection()
{
	FlatSet{sz} a = range_set(0, 10);
	FlatSet{sz} b = range_set(5, 15);

	FlatSet{sz} res = a.intersection(tmem, &b);
	defer res.free();
	test::eq(res.contains(5), true);
	test::eq(res.contains(9), true);
	test::eq(res.contains(4), false);
	test::eq(res.contains(10), false);
	test::eq(res.tvalues().len, (sz)5);

	FlatSet{sz} tres = a.tintersection(&b);
	defer tres.free();
	test::eq(tres.contains(9), true);
	test::eq(tres.contains(10), false);
}

fn void test_difference()
{
	FlatSet{sz} a = range_set(0, 10);
	FlatSet{sz} b = range_set(5, 15);

	FlatSet{sz} res = a.difference(tmem, &b);
	defer res.free();
	test::eq(res.contains(0), true);
	test::eq(res.contains(4), true);
	test::eq(res.contains(5), false);
	test::eq(res.contains(9), false);
	test::eq(res.tvalues().len, (sz)5);
}

fn void test_symmetric_difference()
{
	FlatSet{sz} a = range_set(0, 10);
	FlatSet{sz} b = range_set(5, 15);

	FlatSet{sz} res = a.symmetric_difference(tmem, &b);
	defer res.free();
	test::eq(res.contains(0), true);
	test::eq(res.contains(4), true);
	test::eq(res.contains(5), false);
	test::eq(res.contains(9), false);
	test::eq(res.contains(10), true);
	test::eq(res.contains(14), true);
	test::eq(res.tvalues().len, (sz)10);
}

fn void test_is_subset()
{
	FlatSet{sz} a = range_set(0, 5);
	FlatSet{sz} b = range_set(0, 10);
	FlatSet{sz} c = range_set(5, 10);

	test::eq(a.is_subset(&b), true);
	test::eq(c.is_subset(&b), true);
	test::eq(b.is_subset(&a), false);

	// A set is a subset of itself.
	test::eq(a.is_subset(&a), true);

	// The empty set is a subset of any set.
	FlatSet{sz} empty;
	empty.tinit();
	test::eq(empty.is_subset(&b), true);

	// Disjoint sets are not subsets.
	test::eq(c.is_subset(&a), false);
}

fn void test_values()
{
	FlatSet{sz} set = range_set(0, 8);
	bool removed    = set.remove(3);
	test::eq(removed, true);

	sz[] values  = set.values(tmem);
	sz[] tvalues = set.tvalues();
	test::eq(values.len, (sz)7);
	test::eq(tvalues.len, (sz)7);

	// Sum of 0..7 (28) minus 3 = 25. Order is unspecified.
	sz sum = 0;
	foreach (v : values) sum += v;
	test::eq(sum, (sz)25);
}

fn void test_each()
{
	FlatSet{sz} set = range_set(0, 10);

	sz count = 0;
	sz sum   = 0;
	set.@each(; sz value)
	{
		count++;
		sum += value;
	};
	test::eq(count, (sz)10);
	test::eq(sum, (sz)45);
}

module flatset_test;
import std::collections::flatset;

fn FlatSet{sz} range_set(sz start, sz end)
{
	FlatSet{sz} set;
	set.tinit();
	for (sz i = start; i < end; ++i)
	{
		set.add(i);
	}
	return set;
}
