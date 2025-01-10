module test;
import std::io;
import std::collections::map;

def IntMap = HashMap(<String, int>);
fn void copy_map() @test
{
	TrackingAllocator alloc;
	alloc.init(allocator::heap());
	assert(alloc.allocated() == 0);
    mem::@scoped(&alloc)
	{
		IntMap x;
		x.new_init();
		DString y;
		y.append("hello");
		x.set(y.str_view(), 123);
		y.append("bye");
		x.set(y.str_view(), 333);
		y.clear();
		y.append("bye");
		x.set(y.str_view(), 444);
		assert(x.get("hello")!! == 123);
		assert(x.get("hellobye")!! == 333);
		assert(x.get("bye")!! == 444);
		assert(alloc.allocated() > 0);
		x.free();
		y.free();
		assert(alloc.allocated() == 0);
	};
}

/*
	Some Keys (including Strings) are deep_copied into the hashmap on insertion.
	When copying keys out, the keys themselves must also be deep-copied out.
	Otherwise when the map is freed, the copied-in keys will also be freed,
	resulting in use-after-free.
*/
fn void copy_keys() @test
{
	String[] y;
	@pool() {
		IntMap x;
		x.temp_init();
		x.set("hello", 0); // keys copied into temp hashmap
		y = x.copy_keys(allocator::heap()); // keys copied out
		// end of pool: hashmap and its copied-in keys dropped
	};
	assert(y == {"hello"});
}
