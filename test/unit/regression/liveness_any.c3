module liveness;

protocol TestProto
{
	fn void tesT(&self);
}

fn void Test.tesT(&self) : TestProto
{}

struct Test
{
	void* abc;
}

fn void reflect_test() @test
{
	TestProto* b = malloc(Test);
	b.tesT();
}