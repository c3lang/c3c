module std::core::runtime @if(env::BENCHMARKING);
import libc, std::time, std::io, std::sort, std::math, std::collections;

const DEFAULT_WARMUP_ITERS @local = 3;
const DEFAULT_MAX_ITERS @local = 10000;

faultdef MISSING_CHOICE, INVALID_CHOICE @local;

alias BenchmarkFn = fn void ();

BenchContext bench_context @local = {
	.warmup_iterations = DEFAULT_WARMUP_ITERS,
	.max_iterations = DEFAULT_MAX_ITERS,
};

fn void bench_init_context() @init
{
	if (!bench_context.fn_iters.is_initialized()) bench_context.fn_iters.init(mem);
	if (!bench_context.module_iters.is_initialized()) bench_context.module_iters.init(mem);
	if (!bench_context.module_warmup.is_initialized()) bench_context.module_warmup.init(mem);
}

struct BenchContext
{
	int warmup_iterations;
	int max_iterations;
	HashMap { String, int } fn_iters;
	HashMap { String, int } module_iters;
	HashMap { String, int } module_warmup;
	Clock clock;
	NanoDuration nano_seconds;
	long cycle_start;
	long cycle_stop;
	DString log;
	bool is_warming;
	int this_iteration;
	bool stop;
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
	BenchmarkUnit[] benchmarks = alloc::alloc_array(allocator, BenchmarkUnit, names.len);

	foreach (i, benchmark : fns)
	{
		benchmarks[i] = { names[i], fns[i] };
	}
	return benchmarks;
}

fn double get_stddev(NanoDuration[] set, double mean) @local
{
	double sum_sq_diff = 0;
	foreach (t : set)
	{
		double diff = (double)t - mean;
		sum_sq_diff += diff * diff;
	}
	return math::sqrt(sum_sq_diff / (double)set.len);
}

fn void set_benchmark_module_warmup_iterations(String module_name, int value) @builtin
{
	if (!bench_context.module_warmup.is_initialized()) bench_context.module_warmup.init(mem);
	bench_context.module_warmup[module_name] = value;
}

fn void set_benchmark_module_max_iterations(String module_name, int value) @builtin
{
	assert(value > 0);
	if (!bench_context.module_iters.is_initialized()) bench_context.module_iters.init(mem);
	bench_context.module_iters[module_name] = value;
}

macro void set_benchmark_warmup_iterations(int value) @builtin
{
	set_benchmark_module_warmup_iterations($$MODULE, value);
}

macro void set_benchmark_max_iterations(int value) @builtin
{
	set_benchmark_module_max_iterations($$MODULE, value);
}

fn void set_benchmark_func_iterations(String func, int value) @builtin
{
	assert(value > 0);
	if (!bench_context.fn_iters.is_initialized()) bench_context.fn_iters.init(mem);
	bench_context.fn_iters[func] = value;
}




macro void @start_benchmark() @builtin
{
	bench_context.clock = clock::now();
	bench_context.cycle_start = $$sysclock();
}

macro void @end_benchmark() @builtin
{
	bench_context.nano_seconds = bench_context.clock.mark();
	bench_context.cycle_stop = $$sysclock();
}

macro void @kill_benchmark(String format, ...) @builtin
{
	@log_benchmark(format, $vasplat);
	bench_context.stop = true;
}

macro void @log_benchmark(msg, args...) @builtin => @pool()
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
		return MISSING_CHOICE~;
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
				return INVALID_CHOICE~;
	}
}

fn void append_duration_colored(DString *ds, NanoDuration dur, bool ansi, String base_color) @local
{
	if (!ansi)
	{
		ds.appendf("%8s", dur.tunit_str());
		return;
	}
	static String[] units = { "ns", "us", "ms", "s" };
	static String[] colors = { "\e[0;96m", "\e[0;92m", "\e[0;93m", "\e[0;91m" };

	double val = (double)dur;
	sz idx = 0;
	while (val > 1000.0 && idx < 3)
	{
		val /= 1000.0;
		idx++;
	}
	ds.appendf("%6.2f%s%s%s", val, colors[idx], units[idx], base_color);
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

	for (sz i = 1; i < args.len; i++)
	{
		switch (args[i])
		{
			case "--ansi":
				if (catch @try(ctx.has_ansi_codes, get_yes_no(args[i..]))) return false;
				i++;

			case "--csv-report":
				ctx.generate_csv = true;
			default:
				io::printfn("Invalid argument: %s", args[i]);
				return false;
		}
	}

	sz max_name;
	foreach (&unit : benchmarks) max_name = max(max_name, unit.name.len);
	sz len = max_name + 9;

	DString name = dstring::temp_with_capacity(64);
	name.append_repeat('-', len / 2);
	name.append(" BENCHMARKS ");
	name.append_repeat('-', len - len / 2);

	io::printn(name);
	name.clear();

	DString csv_report;
	csv_report.init(mem);
	defer csv_report.free();

	csv_report.append("Function Name,Iterations,Clock Cycles,Average,Average Nanoseconds,StdDev,StdDev Nanoseconds\n");

	// Find summary widths
	foreach (unit : benchmarks) @pool()
	{
		String module_name = unit.name;
		if (try pos = module_name.rindex_of("::"))
		{
			module_name = module_name[:pos];
		}

		int current_unit_iterations = ctx.fn_iters[unit.name] ?? ctx.module_iters[module_name] ?? ctx.max_iterations;
		int current_warmup_iterations = ctx.module_warmup[module_name] ?? ctx.warmup_iterations;

		if (current_unit_iterations == 0)
		{
			io::printfn("~~~ Skipping unit `%s` (0 iterations count)...", unit.name);
			continue;
		}

		List {NanoDuration} captured_times;
		captured_times.tinit(current_unit_iterations);

		defer name.clear();
		name.appendf("%s ", unit.name);
		name.append_repeat('.', max_name - unit.name.len + 2);
		io::printf("%s ", name.str_view());

		ctx.is_warming = true;
		for (int i = 0; i < current_warmup_iterations; i++)
		{
			unit.func() @inline;
		}
		ctx.is_warming = false;

		NanoDuration running_timer;
		long total_clocks;

		char[] perc_str = { [0..19] = ' ', [20] = 0 };
		int perc = 0;
		int print_step = current_unit_iterations / 100;
		if (print_step == 0) print_step = 1;

		for (ctx.this_iteration = 0; ctx.this_iteration < current_unit_iterations; ++ctx.this_iteration, ctx.nano_seconds = {})
		{
			if (ctx.this_iteration % print_step == 0)   // only print right about when the % will update
			{
				perc_str[0..(int)math::floor((ctx.this_iteration / (float)current_unit_iterations) * 20)] = '#';
				perc = (int)math::ceil(100 * (ctx.this_iteration / (float)current_unit_iterations));

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

		DString stddev_report = dstring::temp_with_capacity(48);
		defer stddev_report.clear();
		double stddev = get_stddev(captured_times.array_view(), (double)measurement);
		stddev_report.append(", ");
		if (ctx.has_ansi_codes) stddev_report.append("\e[0;95m");
		append_duration_colored(&stddev_report, (NanoDuration)stddev, ctx.has_ansi_codes, ctx.has_ansi_codes ? "\e[0;95m" : "");
		if (ctx.has_ansi_codes) stddev_report.append("\e[0m");
		stddev_report.append(" sd");

		io::printf(ctx.has_ansi_codes ? "\r\e[K%s " : "\r%s ", name.str_view());
		if (ctx.has_ansi_codes)
		{
			DString ds = dstring::temp_with_capacity(128);
			ds.append(" \e[0;94m");
			append_duration_colored(&ds, (NanoDuration)measurement, true, "\e[0;94m");
			ds.append("\e[0m avg");
			ds.append(stddev_report.str_view());
			ds.appendf(", %11.2f CPU clocks, %7d iters in \e[0;93m", clock_cycles, current_unit_iterations);
			append_duration_colored(&ds, running_timer, true, "\e[0;93m");
			ds.append("\e[0m");
			io::printn(ds.str_view());
		}
		else
		{
			io::printfn(" %8s avg%s, %11.2f CPU clocks, %7d iters in %8s",
				((NanoDuration)measurement).tunit_str(),
				stddev_report.str_view(),
				clock_cycles,
				current_unit_iterations,
				running_timer.tunit_str()
			);
		}

		if (ctx.generate_csv)
		{
			csv_report.appendf(
				"%s,%d,%f,%s,%d",
				unit.name,
				current_unit_iterations,
				clock_cycles,
				((NanoDuration)measurement).tunit_str(),
				(long)measurement,
			);
			csv_report.appendf(",%s,%d", ((NanoDuration)stddev).tunit_str(), (long)stddev);
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
