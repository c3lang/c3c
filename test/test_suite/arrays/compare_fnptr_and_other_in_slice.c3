extern fn int a(BenchmarkFn[] );
struct Aa
{
	int x;
}
fn int Aa.a(b)
{
	a(""); // #error: It is not possible to cast 'String' to 'BenchmarkFn[]' (fn void()[])
}