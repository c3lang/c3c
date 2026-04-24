module matrix_bench;
import std::math;
import std::time::clock;

struct BenchData
{
	float[<4>] v4;
	Mat4 m1;
	Mat4 m2;
}

BenchData data;
float global_sink = 0.0f;

fn void initialize() @init
{
	ulong t = (ulong)clock::now();
	float f = (float)(t % 1000) * 0.001f;

	data.v4 = { 1.0f, 2.0f, 3.0f, 1.0f + f };
	data.m1 = matrix::IDENTITY4;
	data.m2 = matrix::IDENTITY4;
	data.m1.m00 = 2.0f + f;
	data.m1.m03 = 1.5f;
	data.m2.m11 = 2.0f - f;
	data.m2.m12 = 0.5f;

	set_benchmark_warmup_iterations(10);
	set_benchmark_max_iterations(100_000);

	set_benchmark_func_iterations($reflect(mul).qname, 2_800_000);
	set_benchmark_func_iterations($reflect(vec_mul).qname, 2_100_000);
	set_benchmark_func_iterations($reflect(inverse).qname, 1_700_000);
	set_benchmark_func_iterations($reflect(perspective).qname, 2_200_000);
	set_benchmark_func_iterations($reflect(lookat).qname, 650_000);
}

fn void sink(float f) @noinline
{
	if (f > 1e10) global_sink += f;
}


module matrix_bench @benchmark;

import std::math;

fn void mul()
{
	@start_benchmark();
	Matrix4f res = data.m1.mul(data.m2);
	@end_benchmark();
	sink(res.m00);
}

fn void vec_mul()
{
	@start_benchmark();
	float[<4>] res = data.m1 * data.v4;
	@end_benchmark();
	sink(res[0]);
}

fn void inverse()
{
	@start_benchmark();
	Matrix4f res = data.m1.inverse()!!;
	@end_benchmark();
	sink(res.m00);
}

fn void perspective()
{
	@start_benchmark();
	Mat4 res = matrix::perspective(0.785f, 1.3f, 0.1f, 1000.0f);
	@end_benchmark();
	sink(res.m00);
}

fn void lookat()
{
	@start_benchmark();
	Mat4 res = matrix::look_at({4.0f, 5.0f, 20.0f}, {1.0f, 3.0f, 0.0f}, {0.0f, 1.0f, 0.0f});
	@end_benchmark();
	sink(res.m00);
}
