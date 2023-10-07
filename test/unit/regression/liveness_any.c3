module liveness;

protocol TestProto
{
	fn void tesT();
}

fn void Test.tesT(&self) @dynamic
{}

struct Test (TestProto)
{
	void* abc;
}

fn void reflect_test() @test
{
	TestProto* b = malloc(Test);
	b.tesT();
}