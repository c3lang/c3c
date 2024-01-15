module liveness;

interface TestProto
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
	TestProto* b = mem::alloc(Test);
	b.tesT();
}