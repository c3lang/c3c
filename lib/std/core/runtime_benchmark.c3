module std::core::runtime;
import libc, std::time, std::io, std::sort;

alias BenchmarkFn = fn void();

struct BenchmarkUnit
{
	String name;
	BenchmarkFn func;
}

fn BenchmarkUnit[] benchmark_collection_create(Allocator allocator)
{
	BenchmarkFn[] fns = $$BENCHMARK_FNS;
	String[] names = $$BENCHMARK_NAMES;
	BenchmarkUnit[] benchmarks = allocator::alloc_array(allocator, BenchmarkUnit, names.len);
	foreach (i, benchmark : fns)
	{
		benchmarks[i] = { names[i], fns[i] };
	}
	return benchmarks;
}

const DEFAULT_BENCHMARK_WARMUP_ITERATIONS = 3;
const DEFAULT_BENCHMARK_MAX_ITERATIONS = 10000;

uint benchmark_warmup_iterations @private = DEFAULT_BENCHMARK_WARMUP_ITERATIONS;
uint benchmark_max_iterations @private = DEFAULT_BENCHMARK_MAX_ITERATIONS;

fn void set_benchmark_warmup_iterations(uint value) @builtin
{
    benchmark_warmup_iterations = value;
}

fn void set_benchmark_max_iterations(uint value) @builtin
{
    assert(value > 0);
    benchmark_max_iterations = value;
}

fn bool run_benchmarks(BenchmarkUnit[] benchmarks)
{
	usz max_name;

	foreach (&unit : benchmarks)
	{
		if (max_name < unit.name.len) max_name = unit.name.len;
	}

	usz len = max_name + 9;

	DString name = dstring::temp_with_capacity(64);
	name.append_repeat('-', len / 2);
	name.append(" BENCHMARKS ");
	name.append_repeat('-', len - len / 2);

	io::printn(name);

	name.clear();

	long sys_clock_started;
	long sys_clock_finished;
	long sys_clocks;
	Clock clock;

	foreach(unit : benchmarks)
	{
		defer name.clear();
		name.appendf("Benchmarking %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		for (uint i = 0; i < benchmark_warmup_iterations; i++)
		{
			unit.func() @inline;
		}

		clock = std::time::clock::now();
		sys_clock_started = $$sysclock();

		for (uint i = 0; i < benchmark_max_iterations; i++)
		{
			unit.func() @inline;
		}

		sys_clock_finished = $$sysclock();
		NanoDuration nano_seconds = clock.mark();
		sys_clocks = sys_clock_finished - sys_clock_started;

		io::printfn("[COMPLETE] %.2f ns, %.2f CPU's clocks", (float)nano_seconds / benchmark_max_iterations, (float)sys_clocks / benchmark_max_iterations);
	}

	io::printfn("\n%d benchmark%s run.\n", benchmarks.len, benchmarks.len > 1 ? "s" : "");
	return true;
}

fn bool default_benchmark_runner(String[] args) => @pool()
{
	return run_benchmarks(benchmark_collection_create(tmem));
}
