module vector_bench;
import std::math;
import std::time::clock;

struct BenchData
{
	float[<3>] v1;
	float[<3>] v2;
}

BenchData data;
float global_sink = 0.0f;

fn void initialize() @init
{
	ulong t = (ulong)clock::now();
	float f = (float)(t % 1000) * 0.001f;

	data.v1 = { 1.2f + f, 3.4f, 5.6f };
	data.v2 = { 7.8f, 9.0f + f, 1.2f };

	set_benchmark_warmup_iterations(10);
	set_benchmark_max_iterations(100_000);

	set_benchmark_func_iterations($reflect(add).qname, 4_500_000);
	set_benchmark_func_iterations($reflect(dot).qname, 4_500_000);
	set_benchmark_func_iterations($reflect(cross).qname, 2_000_000);
	set_benchmark_func_iterations($reflect(normalize).qname, 2_800_000);
}

fn void sink(float f) @noinline
{
	if (f > 1e10) global_sink += f;
}


module vector_bench @benchmark;

import std::math;

fn void add()
{
	@start_benchmark();
	float[<3>] res = data.v1 + data.v2;
	@end_benchmark();
	sink(res[0]);
}

fn void dot()
{
	@start_benchmark();
	float res = data.v1.dot(data.v2);
	@end_benchmark();
	sink(res);
}

fn void cross()
{
	@start_benchmark();
	float[<3>] res = data.v1.cross(data.v2);
	@end_benchmark();
	sink(res[0]);
}

fn void normalize()
{
	@start_benchmark();
	float[<3>] res = data.v1.normalize();
	@end_benchmark();
	sink(res[0]);
}
