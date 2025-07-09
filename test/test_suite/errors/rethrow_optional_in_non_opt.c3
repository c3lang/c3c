module test;
import std;
fn int? test() => 1;

fn int main()
{
	test()!; // #error: Did you intend to use '!!' instead
	int? a = test()!; // #error: maybe you added '!' by mistake?
	return 0;
}
