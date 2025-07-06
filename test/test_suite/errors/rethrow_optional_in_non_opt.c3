module test;
import std;
fn int? test() => 1;

fn int main()
{
	test()!; // #error: This expression is doing a rethrow
	return 0;
}
