module quaternion_bench;
import std::math;
import std::time::clock;

struct BenchData
{
	Quat q1;
	Quat q2;
	float[<3>] v3;
}

BenchData data;
float global_sink = 0.0f;

fn void initialize() @init
{
	ulong t = (ulong)clock::now();
	float f = (float)(t % 1000) * 0.001f;

	data.q1 = quaternion::IDENTITY;
	data.q2 = quaternion::IDENTITY;
	data.q1.l = 0.707f + f;
	data.q1.i = 0.707f;
	data.q2.l = 0.707f - f;
	data.q2.j = 0.707f;
	data.v3 = { 1.0f, 0.0f, 0.0f };

	set_benchmark_warmup_iterations(10);
	set_benchmark_max_iterations(100_000);

	set_benchmark_func_iterations($reflect(mul).qname, 1_350_000);
	set_benchmark_func_iterations($reflect(rotate).qname, 600_000);
	set_benchmark_func_iterations($reflect(normalize).qname, 2_600_000);
	set_benchmark_func_iterations($reflect(slerp).qname, 1_500_000);
	set_benchmark_func_iterations($reflect(to_matrix).qname, 1_500_000);
}

fn void sink(float f) @noinline
{
	if (f > 1e10) global_sink += f;
}


module quaternion_bench @benchmark;

import std::math;

fn void mul()
{
	@start_benchmark();
	Quat res = data.q1 * data.q2;
	@end_benchmark();
	sink(res.l);
}

fn void rotate()
{
	@start_benchmark();
	float[<3>] res = data.q1.rotate_vec3(data.v3);
	@end_benchmark();
	sink(res[0]);
}

fn void normalize()
{
	@start_benchmark();
	Quaternionf res = data.q1.normalize();
	@end_benchmark();
	sink(res.l);
}

fn void slerp()
{
	@start_benchmark();
	Quaternionf res = data.q1.slerp(data.q2, 0.5f);
	@end_benchmark();
	sink(res.l);
}

fn void to_matrix()
{
	@start_benchmark();
	Matrix4f res = data.q1.to_matrix();
	@end_benchmark();
	sink(res.m00);
}
