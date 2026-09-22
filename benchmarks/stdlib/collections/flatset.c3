module flatmap_bench;
import std::math::random;

const ITERATIONS = 10;

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(2);
	set_benchmark_max_iterations(ITERATIONS);

	set_benchmark_func_iterations($reflect(hashset_operations).qname, ITERATIONS);
	set_benchmark_func_iterations($reflect(flatset_operations).qname, ITERATIONS);
}

fn String generate_key(sz key_len)
{
	String key = (String)mem::temp_array(char, key_len);
	foreach (&c : key)
	{
		*c = (char)random::rand('z' - 'a' + 1) + 'a';
	}
	return key;
}

module flatmap_bench @benchmark;

import std::collections::flatset;
import std::collections::set;
import std::collections::list;
import std::io @norecurse;

const sz ITEM_COUNT     = 500_000;
const sz KEY_LEN        = 64;
const sz PREALLOC_COUNT = 8;

fn void hashset_operations() => @pool()
{
	HashSet{String} set;
	set.init(tmem, PREALLOC_COUNT);

	List{String} keys;
	keys.init(tmem, ITEM_COUNT);

	for (sz i = 0; i < ITEM_COUNT; ++i)
	{
		keys.push(generate_key(KEY_LEN));
	}

	@start_benchmark();

	foreach (s : keys)
	{
		set.add(s);
	}

	bool contains_item @noinit;

	foreach (s : keys)
	{
		contains_item = set.contains(s);
		if (!contains_item)
		{
			io::printn("Error happened: contains");
			abort();
		}
	}

	foreach (s : keys)
	{
		set.remove(s)!!;
	}

	foreach (s : keys)
	{
		contains_item = set.contains(s);
		if (contains_item)
		{
			io::printn("Error happened: contains (2)");
			abort();
		}
	}

	@end_benchmark();
}

fn void flatset_operations() => @pool()
{
	FlatSet{String} set;
	set.init(tmem, PREALLOC_COUNT);

	List{String} keys;
	keys.init(tmem, ITEM_COUNT);

	for (sz i = 0; i < ITEM_COUNT; ++i)
	{
		keys.push(generate_key(KEY_LEN));
	}

	@start_benchmark();

	foreach (s : keys)
	{
		set.add(s);
	}

	bool contains_item @noinit;

	foreach (s : keys)
	{
		contains_item = set.contains(s);
		if (!contains_item)
		{
			io::printn("Error happened: contains (2)");
			abort();
		}
	}

	foreach (s : keys)
	{
		if (!set.remove(s))
		{
			io::printfn("Error happened: remove");
			abort();
		}
	}

	foreach (s : keys)
	{
		contains_item = set.contains(s);
		if (contains_item)
		{
			io::printn("Error happened: contains (2)");
			abort();
		}
	}

	@end_benchmark();
}
