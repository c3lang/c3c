module math_bench;
import std::math;
import std::time::clock;

struct BenchData
{
	float f1, f2;
}

BenchData data;
double global_sink = 0.0;

fn void initialize() @init
{
	ulong t = (ulong)clock::now();
	float f = (float)(t % 1000) * 0.001f;

	data.f1 = 0.5f + f;
	data.f2 = 1.6f - f;

	set_benchmark_warmup_iterations(5);
	set_benchmark_max_iterations(100_000);

	set_benchmark_func_iterations($qnameof(sin), 4_500_000);
	set_benchmark_func_iterations($qnameof(cos), 4_500_000);
	set_benchmark_func_iterations($qnameof(sqrt), 4_500_000);
	set_benchmark_func_iterations($qnameof(log10), 3_800_000);
	set_benchmark_func_iterations($qnameof(pow), 3_500_000);
	set_benchmark_func_iterations($qnameof(atan2), 3_100_000);
}

fn void sink(double d) @noinline
{
	if (d > 1e15) global_sink += d;
}


module math_bench @benchmark;

import std::math;

fn void sin()
{
	@start_benchmark();
	float res = math::sin(data.f1);
	@end_benchmark();
	sink((double)res);
}

fn void cos()
{
	@start_benchmark();
	float res = math::cos(data.f1);
	@end_benchmark();
	sink((double)res);
}

fn void sqrt()
{
	@start_benchmark();
	float res = math::sqrt(data.f2);
	@end_benchmark();
	sink((double)res);
}

fn void log10()
{
	@start_benchmark();
	float res = math::log10(data.f2);
	@end_benchmark();
	sink((double)res);
}

fn void pow()
{
	@start_benchmark();
	float res = math::pow(data.f1, data.f2);
	@end_benchmark();
	sink((double)res);
}

fn void atan2()
{
	@start_benchmark();
	float res = math::atan2(data.f1, data.f2);
	@end_benchmark();
	sink((double)res);
}
