module std::core::runtime;
import libc, std::time, std::io, std::sort, std::math, std::collections::map;

alias BenchmarkFn = fn void ();

HashMap { String, uint } bench_fn_iters @local;

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
		if (!bench_fn_iters.has_key(names[i])) bench_fn_iters[names[i]] = benchmark_max_iterations;
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
	foreach (k : bench_fn_iters.key_iter()) bench_fn_iters[k] = value;
}

fn void set_benchmark_func_iterations(String func, uint value) @builtin
{
	assert(value > 0);
	bench_fn_iters[func] = value;
}


Clock benchmark_clock @local;
NanoDuration benchmark_nano_seconds @local;
long cycle_start @local;
long cycle_stop @local;
DString benchmark_log @local;
bool benchmark_warming @local;
uint this_iteration @local;
bool benchmark_stop @local;

macro void @start_benchmark()
{
	benchmark_clock = clock::now();
	cycle_start = $$sysclock();
}

macro void @end_benchmark()
{
	benchmark_nano_seconds = benchmark_clock.mark();
	cycle_stop = $$sysclock();
}

macro void @kill_benchmark(String format, ...)
{
	@log_benchmark(format, $vasplat);
	benchmark_stop = true;
}

macro void @log_benchmark(msg, args...) => @pool()
{
	if (benchmark_warming) return;

	benchmark_log.appendf("%s [%d]:  ", $$FUNC, this_iteration);
	benchmark_log.appendfn(msg, ...args);
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

	foreach (unit : benchmarks)
	{
		defer name.clear();
		name.appendf("Benchmarking %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		benchmark_warming = true;
		for (uint i = 0; i < benchmark_warmup_iterations; i++)
		{
			unit.func() @inline;
		}
		benchmark_warming = false;

		NanoDuration running_timer;
		long total_clocks;

		uint current_benchmark_iterations = bench_fn_iters[unit.name] ?? benchmark_max_iterations;
		char[] perc_str = { [0..19] = ' ', [20] = 0 };
		int perc = 0;
		uint print_step = current_benchmark_iterations / 100;

		for (this_iteration = 0; this_iteration < current_benchmark_iterations; ++this_iteration, benchmark_nano_seconds = {})
		{
			if (0 == this_iteration % print_step)   // only print right about when the % will update
			{
				perc_str[0..(uint)math::floor((this_iteration / (float)current_benchmark_iterations) * 20)] = '#';
				perc = (uint)math::ceil(100 * (this_iteration / (float)current_benchmark_iterations));

				io::printf("\r%s [%s] %d / %d (%d%%)", name.str_view(), (ZString)perc_str, this_iteration, current_benchmark_iterations, perc);
				io::stdout().flush()!!;
			}

			@start_benchmark();   // can be overridden by calls inside the unit's func

			unit.func() @inline;
			if (benchmark_stop) return false;

			if (benchmark_nano_seconds == (NanoDuration){}) @end_benchmark();   // only mark when it wasn't already by the unit.func

			total_clocks += cycle_stop - cycle_start;
			running_timer += benchmark_nano_seconds;
		}

		float clock_cycles = (float)total_clocks / current_benchmark_iterations;
		float measurement = (float)running_timer / current_benchmark_iterations;
		String[] units = { "nanoseconds", "microseconds", "milliseconds", "seconds" };

		float adjusted_measurement = measurement;
		while (adjusted_measurement > 1_000) adjusted_measurement /= 1_000;
		float adjusted_runtime_total = (float)running_timer;
		while (adjusted_runtime_total > 1_000) adjusted_runtime_total /= 1_000;

		io::printf("\r%s ", name.str_view());
		io::printfn(
			"[COMPLETE] %.2f %s, %.2f CPU clocks, %d iterations (runtime %.2f %s)",
			adjusted_measurement,
			units[math::min(3, (int)math::floor(math::log(measurement, 1_000)))],
			clock_cycles,
			current_benchmark_iterations,
			adjusted_runtime_total,
			units[math::min(3, (int)math::floor(math::log((float)running_timer, 1_000)))],
		);
	}

	io::printfn("\n%d benchmark%s run.\n", benchmarks.len, benchmarks.len > 1 ? "s" : "");
	return true;
}

fn bool default_benchmark_runner(String[] args) => @pool()
{
	benchmark_log.init(mem);
	defer
	{
		if (benchmark_log.len()) io::printfn("\n---------- BENCHMARK LOG ----------\n%s\n", benchmark_log.str_view());
		benchmark_log.free();
	}

	return run_benchmarks(benchmark_collection_create(tmem));
}
