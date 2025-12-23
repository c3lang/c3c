module std::core::runtime;
import libc, std::time, std::io, std::sort, std::math, std::collections;

const DEFAULT_WARMUP_ITERS @local = 3;
const DEFAULT_MAX_ITERS @local = 10000;

faultdef MISSING_CHOICE, INVALID_CHOICE @local;

alias BenchmarkFn = fn void ();

BenchContext bench_context @local = {
	.warmup_iterations = DEFAULT_WARMUP_ITERS,
	.max_iterations = DEFAULT_MAX_ITERS,
};

struct BenchContext
{
	uint warmup_iterations;
	uint max_iterations;
	HashMap { String, uint } fn_iters;
	Clock clock;
	NanoDuration nano_seconds;
	long cycle_start;
	long cycle_stop;
	DString log;
	bool is_warming;
	uint this_iteration;
	bool stop;
	bool report_median;
	bool has_ansi_codes;
	bool generate_csv;
}

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
		if (!bench_context.fn_iters.has_key(names[i]))
		{
			bench_context.fn_iters[names[i]] = bench_context.max_iterations;
		}
	}
	return benchmarks;
}

<*
 @require $defined((double)set[0])
 @require lengthof(set) > 0
*>
macro get_median(set) @local
	=> lengthof(set) % 2 != 0
		? (double)set[lengthof(set) / 2]
		: ((double)(set[lengthof(set) / 2 - 1] + set[lengthof(set) / 2]) / 2.0);

fn void set_benchmark_warmup_iterations(uint value) @builtin
{
	bench_context.warmup_iterations = value;
}

fn void set_benchmark_max_iterations(uint value) @builtin
{
	assert(value > 0);
	bench_context.max_iterations = value;
	foreach (k : bench_context.fn_iters.key_iter()) bench_context.fn_iters[k] = value;
}

fn void set_benchmark_func_iterations(String func, uint value) @builtin
{
	assert(value > 0);
	bench_context.fn_iters[func] = value;
}

fn void set_benchmark_report_median(bool does) @builtin
{
	bench_context.report_median = does;
}


macro void @start_benchmark()
{
	bench_context.clock = clock::now();
	bench_context.cycle_start = $$sysclock();
}

macro void @end_benchmark()
{
	bench_context.nano_seconds = bench_context.clock.mark();
	bench_context.cycle_stop = $$sysclock();
}

macro void @kill_benchmark(String format, ...)
{
	@log_benchmark(format, $vasplat);
	bench_context.stop = true;
}

macro void @log_benchmark(msg, args...) => @pool()
{
	if (bench_context.is_warming) return;

	bench_context.log.appendf("%s [%d]:  ", $$FUNC, bench_context.this_iteration);
	bench_context.log.appendfn(msg, ...args);
}


fn bool? get_yes_no(String[] args_at)
{
	if (args_at.len < 2)
	{
		io::printfn("Missing argument for `%s` - expected 'yes' or 'no'.", args_at[0]);
		return MISSING_CHOICE?;
	}
	switch (args_at[1].to_lower_copy(tmem))
	{
		case "on":
		case "yes":
			return true;
		case "off":
		case "no":
			return false;
		default:
			io::printfn("Missing argument for `%s` - expected 'yes' or 'no'.", args_at[0]);
			return INVALID_CHOICE?;
	}
}

fn bool run_benchmarks(String[] args, BenchmarkUnit[] benchmarks)
{
	if (!benchmarks.len)
	{
		io::printn("There are no benchmark units to run.");
		return true;
	}

	BenchContext* ctx = &bench_context;
	ctx.has_ansi_codes = terminal_has_ansi_codes();

	for (usz i = 1; i < args.len; i++)
	{
		switch (args[i])
		{
			case "--ansi":
				if (catch @try(ctx.has_ansi_codes, get_yes_no(args[i..]))) return false;
				i++;
			case "--median":
				if (catch @try(ctx.report_median, get_yes_no(args[i..]))) return false;
				i++;
			case "--csv-report":
				ctx.generate_csv = true;
			default:
				io::printfn("Invalid argument: %s", args[i]);
				return false;
		}
	}

	usz max_name;
	foreach (&unit : benchmarks) max_name = max(max_name, unit.name.len);
	usz len = max_name + 9;

	DString name = dstring::temp_with_capacity(64);
	name.append_repeat('-', len / 2);
	name.append(" BENCHMARKS ");
	name.append_repeat('-', len - len / 2);

	io::printn(name);
	name.clear();

	DString csv_report;
	csv_report.init(mem);
	defer csv_report.free();

	csv_report.append("Function Name,Iterations,Clock Cycles,Average,Average Nanoseconds");
	if (ctx.report_median) csv_report.append(",Median,Median Nanoseconds");
	csv_report.append("\n");

	foreach (unit : benchmarks) @pool()
	{
		uint current_unit_iterations = ctx.fn_iters[unit.name] ?? ctx.max_iterations;
		if (current_unit_iterations == 0)
		{
			io::printfn("~~~ Skipping unit `%s` (0 iterations count)...", unit.name);
			continue;
		}

		List {NanoDuration} captured_times;
		captured_times.tinit();

		defer name.clear();
		name.appendf("Benchmarking %s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		ctx.is_warming = true;
		for (uint i = 0; i < ctx.warmup_iterations; i++)
		{
			unit.func() @inline;
		}
		ctx.is_warming = false;

		NanoDuration running_timer;
		long total_clocks;

		char[] perc_str = { [0..19] = ' ', [20] = 0 };
		int perc = 0;
		uint print_step = current_unit_iterations / 100;

		for (ctx.this_iteration = 0; ctx.this_iteration < current_unit_iterations; ++ctx.this_iteration, ctx.nano_seconds = {})
		{
			if (print_step && 0 == ctx.this_iteration % print_step)   // only print right about when the % will update
			{
				perc_str[0..(uint)math::floor((ctx.this_iteration / (float)current_unit_iterations) * 20)] = '#';
				perc = (uint)math::ceil(100 * (ctx.this_iteration / (float)current_unit_iterations));

				io::printf(
					ctx.has_ansi_codes
						? "\r\e[97m%s\e[0m [\e[0;32m%s\e[0m] %d / %d (\e[0;96m%d%%\e[0m) "
						: "\r%s [%s] %d / %d (%d%%) ",
					name.str_view(), (ZString)perc_str, ctx.this_iteration, current_unit_iterations, perc
				);
				io::stdout().flush()!!;
			}

			@start_benchmark();   // can be overridden by calls inside the unit's func

			unit.func() @inline;
			if (ctx.stop) return false;

			if (ctx.nano_seconds == (NanoDuration){}) @end_benchmark();   // only mark when it wasn't already by the unit.func

			total_clocks += ctx.cycle_stop - ctx.cycle_start;
			running_timer += ctx.nano_seconds;
			captured_times.push(ctx.nano_seconds);
		}

		float clock_cycles = (float)total_clocks / current_unit_iterations;
		float measurement = (float)running_timer / current_unit_iterations;

		DString median_report = dstring::temp_with_capacity(48);
		defer median_report.clear();
		if (ctx.report_median)
		{
			io::printf(ctx.has_ansi_codes ? "  [\e[0;31msorting results...\e[0m]  " : "  [sorting results...]  "); io::stdout().flush()!!;
			quicksort(&captured_times, fn int (NanoDuration a, NanoDuration b) => (int)(a - b));
			median_report.appendf(ctx.has_ansi_codes ? " avg, \e[0;95m%8s\e[0m med" : " avg, %8s med", ((NanoDuration)get_median(captured_times)).tunit_str());
		}

		io::printf("\r%s ", name.str_view());
		io::printfn(
			ctx.has_ansi_codes
				? " \e[0;94m%8s\e[0m%s, %10.2f CPU clocks, %d iters in \e[0;93m%s\e[0m"
				: " %8s%s, %10.2f CPU clocks, %d iters in %s",
			((NanoDuration)measurement).tunit_str(),
			median_report.str_view(),
			clock_cycles,
			current_unit_iterations,
			running_timer.tunit_str()
		);

		if (ctx.generate_csv)
		{
			csv_report.appendf(
				"%s,%d,%d,%s,%d",
				unit.name,
				current_unit_iterations,
				clock_cycles,
				((NanoDuration)measurement).tunit_str(),
				(long)measurement,
			);
			if (ctx.report_median) csv_report.appendf(",%s,%d", ((NanoDuration)get_median(captured_times)).tunit_str(), (long)get_median(captured_times));
			csv_report.append("\n");
		}
	};

	io::printfn("\n%d benchmark%s run.\n", benchmarks.len, benchmarks.len > 1 ? "s" : "");

	if (ctx.generate_csv)
	{
		name.append_repeat('-', len / 2);
		name.append(" CSV REPORT ");
		name.append_repeat('-', len - len / 2);
		io::printn(name);
		name.clear();
		io::print(csv_report);
		io::print("\n");
	}

	return true;
}

fn bool default_benchmark_runner(String[] args)
{
	bench_context.log.init(mem);
	defer
	{
		if (bench_context.log.len()) io::printfn("\n---------- BENCHMARK LOG ----------\n%s\n", bench_context.log.str_view());
		bench_context.log.free();
	}

	return run_benchmarks(args, benchmark_collection_create(tmem));
}
