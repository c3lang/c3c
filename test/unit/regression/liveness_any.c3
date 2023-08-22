module liveness;

fn void any.tesT(void*) @interface;
fn void Test.tesT(&self) @dynamic
{}

struct Test
{
	void* abc;
}

fn void reflect_test() @test
{
	any b = malloc(Test);
	b.tesT();
}