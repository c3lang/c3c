module test;
import std::io;
import std::collections::map;

fn void! copy_map() @test
{
	TrackingAllocator alloc;
	alloc.init(mem::heap());
	assert(alloc.allocated() == 0);
    mem::@scoped(&alloc)
	{
		HashMap(<String, int>) x;
		x.init();
		DString y;
		y.append("hello");
		x.set(y.as_str(), 123);
		y.append("bye");
		x.set(y.as_str(), 333);
		y.clear();
		y.append("bye");
		x.set(y.as_str(), 444);
		assert(x.get("hello")! == 123);
		assert(x.get("hellobye")! == 333);
		assert(x.get("bye")! == 444);
		assert(alloc.allocated() > 0);
		x.free();
		y.free();
		assert(alloc.allocated() == 0);
	};
}