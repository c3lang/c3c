module linkedlist_benchmarks;

import std::collections::linkedlist;


LinkedList{int} long_list;
const HAY = 2;
const NEEDLE = 1000;

fn void bench_setup() @init
{
	set_benchmark_warmup_iterations(3);
	set_benchmark_max_iterations(4096);

    int[*] haystack = { [0..999] = HAY };
    long_list = linkedlist::@new{int}(mem, haystack[..]);
    long_list.push(NEEDLE);
    long_list.push_all(haystack[..]);
}


// ==============================================================================================
module linkedlist_benchmarks @benchmark;

String die_str = "Failed to find the value `1`. Is something broken?";


fn void foreach_iterator()
{
    foreach (v : long_list.array_view()) if (v == NEEDLE) return;
    runtime::@kill_benchmark(die_str);
}

fn void foreach_r_iterator()
{
    foreach_r (v : long_list.array_view()) if (v == NEEDLE) return;
    runtime::@kill_benchmark(die_str);
}
