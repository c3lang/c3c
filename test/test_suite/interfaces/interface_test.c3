module test;
import std;

fn int main()
{
	return 0;
}

module foo @test;
interface Bob
{
	fn void test();
}
struct Foo
{
	int a;
}
fn void Foo.test(&self) {}
