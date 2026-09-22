module flatmap_bench;
import std::math::random;

const ITERATIONS = 10;

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(2);
	set_benchmark_max_iterations(ITERATIONS);

	set_benchmark_func_iterations($reflect(hashmap_operations).qname, ITERATIONS);
	set_benchmark_func_iterations($reflect(flatmap_operations).qname, ITERATIONS);
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

import std::collections::flatmap;
import std::collections::map;
import std::collections::list;
import std::io @norecurse;

const sz ITEM_COUNT = 1_000_000;
const sz KEY_LEN = 64;
const sz PREALLOC_COUNT = 8; 

fn void hashmap_operations() => @pool()
{
    HashMap{String, sz} map;
    map.init(tmem, PREALLOC_COUNT);

    List{String} keys;
    keys.init(tmem, ITEM_COUNT);

    for (sz i = 0; i < ITEM_COUNT; ++i)
    {
        keys.push(generate_key(KEY_LEN));
    }

	@start_benchmark();

    foreach (i, s : keys)
    {
        map.set(s, i);
    }

    foreach (s : keys)
    {
        map.get(s)!!;
    }

    foreach (s : keys)
    {
        map.remove(s)!!;
    }

    foreach (s : keys)
    {
        sz? res = map.get(s);
        if (try val = res)
        {
            io::printn("Error happened: get (2)");
            abort();
        }
    }

	@end_benchmark();
}

fn void flatmap_operations() => @pool()
{
    FlatMap{String, sz} map;
    map.init(tmem, PREALLOC_COUNT);

    List{String} keys;
    keys.init(tmem, ITEM_COUNT);

    for (sz i = 0; i < ITEM_COUNT; ++i)
    {
        keys.push(generate_key(KEY_LEN));
    }

	@start_benchmark();

    foreach (i, s : keys)
    {
        map.set(s, i);
    }

    foreach (s : keys)
    {
        map.get(s)!!;
    }

    foreach (s : keys)
    {
        if (catch map.remove(s))
        {
            io::printfn("Error happened: remove");
            abort();
        }
    }

    foreach (s : keys)
    {
        sz? res = map.get(s);
        if (try val = res)
        {
            io::printfn("Error happened: get (2)");
            abort();
        }
    }

	@end_benchmark();
}
