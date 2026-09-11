module flatmap_test @test;
import std::collections::flatmap;
import std::collections::list;

fn void test_with_strings()
{
	List{String} keys;
	keys.init(tmem, 100);
	defer keys.free();

	for (sz i = 0; i < 100; ++i)
	{
		keys.push(gen_random_string(tmem, 20));
	}

	FlatMap{String, sz} map;
	map.init(tmem, 100);

	foreach (idx, key : keys)
	{
		map.set(key, idx);
	}

	foreach (idx, key : keys)
	{
		test::eq(map.get(key)!!, idx);
	}
}

fn void test_init_tinit()
{
	FlatMap{sz, sz} map;
	map.init(tmem, 32);
	map.set(1, 10);
	test::eq(map.get(1)!!, (sz)10);

	FlatMap{sz, sz} tmap;
	tmap.tinit();
	tmap.set(5, 50);
	test::eq(tmap.get(5)!!, (sz)50);

	FlatMap{sz, sz} hmap;
	hmap.init(tmem, 16, 0.5f);
	hmap.set(9, 90);
	test::eq(hmap.get(9)!!, (sz)90);
}

fn void test_init_from_keys_and_values()
{
	FlatMap{sz, sz} map;
	map.init_from_keys_and_values(tmem, { 10, 20, 30 }, { 1, 2, 3 });
	test::eq(map.get(10)!!, (sz)1);
	test::eq(map.get(20)!!, (sz)2);
	test::eq(map.get(30)!!, (sz)3);
	test::eq(map.has(40), false);

	FlatMap{sz, sz} tmap;
	tmap.tinit_from_keys_and_values({ 1, 2, 3 }, { 4, 5, 6 });
	test::eq(tmap.get(1)!!, (sz)4);
	test::eq(tmap.get(3)!!, (sz)6);

	// Duplicate keys are overwritten (last wins), count reflects unique keys.
	FlatMap{sz, sz} dmap;
	dmap.tinit_from_keys_and_values({ 1, 1, 2 }, { 10, 20, 30 });
	test::eq(dmap.get(1)!!, (sz)20);
	test::eq(dmap.get(2)!!, (sz)30);
	test::eq(dmap.keys(tmem).len, (sz)2);
}

fn void test_set_get()
{
	FlatMap{sz, sz} map;
	map.tinit();

	// Enough insertions to trigger grow() several times (default capacity 16).
	for (sz i = 0; i < 100; ++i)
	{
		map.set(i, i * 10);
	}
	for (sz i = 0; i < 100; ++i)
	{
		test::eq(map.get(i)!!, i * 10);
	}

	// Overwriting an existing key does not add a new entry.
	map.set(0, 42);
	test::eq(map.get(0)!!, (sz)42);

	// Getting a missing key yields NOT_FOUND.
	test::@error(map.get(10000));
}

fn void test_get_ref()
{
	FlatMap{sz, sz} map;
	map.tinit();
	map.set(7, 100);

	sz* v = map.get_ref(7)!!;
	test::eq(*v, (sz)100);

	// Mutating through the returned reference changes the stored value.
	*v = 999;
	test::eq(map.get(7)!!, (sz)999);

	test::@error(map.get_ref(42));
}

fn void test_has()
{
	FlatMap{sz, sz} map;
	map.tinit();
	map.set(1, 10);
	test::eq(map.has(1), true);
	test::eq(map.has(2), false);
}

fn void test_remove()
{
	FlatMap{sz, sz} map;
	map.tinit();
	map.set(1, 10);
	map.set(2, 20);

	// Removing a present key succeeds.
	if (catch map.remove(1)) return;
	test::eq(map.has(1), false);

	// Removing the same key again yields NOT_FOUND.
	test::@error(map.remove(1));

	// Re-setting after removal reuses the deleted slot.
	map.set(1, 99);
	test::eq(map.get(1)!!, (sz)99);
}

fn void test_clear()
{
	FlatMap{sz, sz} map;
	map.tinit();
	map.set(1, 10);
	map.set(2, 20);

	map.clear();
	test::@error(map.get(1));
	test::eq(map.has(1), false);
	test::eq(map.has(2), false);

	// Map remains usable after clear.
	map.set(3, 30);
	test::eq(map.get(3)!!, (sz)30);
}

fn void test_free()
{
	FlatMap{sz, sz} map;
	map.init(tmem, 32);
	map.set(1, 10);
	test::eq(map.get(1)!!, (sz)10);
	map.free();

	// Map can be re-initialized and reused after free.
	map.init(tmem, 16);
	map.set(2, 20);
	test::eq(map.get(2)!!, (sz)20);
}

fn void test_keys_values()
{
	FlatMap{sz, sz} map;
	map.tinit();
	for (sz i = 0; i < 10; ++i)
	{
		map.set(i, i * 2);
	}
	map.remove(5);

	sz[] keys    = map.keys(tmem);
	sz[] tkeys   = map.tkeys();
	sz[] values  = map.values(tmem);
	sz[] tvalues = map.tvalues();

	test::eq(keys.len, (sz)9);
	test::eq(tkeys.len, (sz)9);
	test::eq(values.len, (sz)9);
	test::eq(tvalues.len, (sz)9);

	// Order is unspecified, so check sums.
	sz key_sum = 0;
	foreach (k : keys) key_sum += k;
	test::eq(key_sum, (sz)40);

	sz value_sum = 0;
	foreach (v : values) value_sum += v;
	test::eq(value_sum, (sz)80);
}

fn void test_each_pair()
{
	FlatMap{sz, sz} map;
	map.tinit();
	for (sz i = 0; i < 10; ++i)
	{
		map.set(i, i * 3);
	}

	sz count = 0;
	sz sum   = 0;
	map.@each_pair(; sz key, sz value)
	{
		count++;
		sum += value;
		test::eq(value, key * 3);
	};
	test::eq(count, (sz)10);
	test::eq(sum, (sz)135);
}

fn void test_each_key()
{
	FlatMap{sz, sz} map;
	map.tinit();
	for (sz i = 0; i < 10; ++i)
	{
		map.set(i, i * 3);
	}

	sz count = 0;
	sz sum   = 0;
	map.@each_key(; sz key)
	{
		count++;
		sum += key;
	};
	test::eq(count, (sz)10);
	test::eq(sum, (sz)45);
}

fn void test_each_value()
{
	FlatMap{sz, sz} map;
	map.tinit();
	for (sz i = 0; i < 10; ++i)
	{
		map.set(i, i * 3);
	}

	sz count = 0;
	sz sum   = 0;
	map.@each_value(; sz value)
	{
		count++;
		sum += value;
	};
	test::eq(count, (sz)10);
	test::eq(sum, (sz)135);
}

fn void test_string_keys()
{
	FlatMap{String, sz} map;
	map.tinit();
	String[] words = { "apple", "banana", "cherry", "date" };
	for (sz i = 0; i < 4; ++i)
	{
		map.set(words[i], i);
	}

	test::eq(map.get("apple")!!, (sz)0);
	test::eq(map.get("cherry")!!, (sz)2);

	if (catch map.remove("banana")) return;
	test::eq(map.has("banana"), false);

	sz[] vals = map.tvalues();
	test::eq(vals.len, (sz)3);
}

module flatmap_test;
import std::math::random;

fn String gen_random_string(Allocator alloc, sz len)
{
	String res = (String)alloc::alloc_array(alloc, char, len);
	foreach (&c : res)
	{
		*c = (char)(random::rand('z' - 'a') + 'a');
	}
	return res;
}
