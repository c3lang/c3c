module foo;
import std;
fn void test(float[<4>] @simd* x)
{}
fn void test2(float[4]* x)
{}

fn int main(String[] args)
{
	float[4] a;
	float[<4>] @simd b;
	test(&a); // #error: Implicitly casting
	test(&b);
	test2(&a);
	test2(&b);
	return 1;
}
