module bitset_test @test;
import std::collections::bitset;
import std::collections::growablebitset;
import std::collections::list;
import std::io;

def List = List(<usz>);

def BitSet = BitSet(<2048>);

fn void set_get()
{
	BitSet bs;
	assert(bs.cardinality() == 0);

	assert(!bs.get(0));
	bs.set(0);
	assert(bs.get(0));
	assert(bs.cardinality() == 1);

	assert(!bs.get(2000));
	bs[2000] = true;
	assert(bs.get(2000));
	assert(bs.cardinality() == 2);

	List found;
	found.temp_init();
	foreach (i, x : bs)
	{
		switch (i)
		{
			case 0:
			case 2000:
				assert(x);
				found.push(i);
			default:
				assert(!x);
		}
	}
	assert(found.array_view() == usz[]{0, 2000});

	bs.unset(0);
	assert(!bs.get(0));
	bs[2000] = false;
	assert(!bs.get(2000));
	assert(bs.cardinality() == 0);
}

def GrowableBitSet = GrowableBitSet(<char>);
fn void growable_set_get()
{
	GrowableBitSet bs;
	bs.temp_init();
	assert(bs.cardinality() == 0, "Invalid cardinality");

	assert(!bs.get(0), "Get was true");
	bs.set(0);
	assert(bs.get(0), "Get should be true");
	assert(bs.cardinality() == 1, "Cardinality should be 1");
	assert(bs.len() == 1, "Len should be 1");

	assert(!bs.get(2000), "Get 2000 should be false");
	bs[2000] = true;
	assert(bs.get(2000), "Get 2000 should be true");
	assert(bs.cardinality() == 2, "Cardinality should be 2");

	assert(bs.data.len() == 251, "Len should be 251");
	assert(bs.len() == 2001, "Len should be 2001");

	List found;
	found.temp_init();
	foreach (i, x : bs)
	{
		switch (i)
		{
			case 0:
			case 2000:
				assert(x);
				found.push(i);
			default:
				assert(!x, "Should not get here");
		}
	}
	assert(found.array_view() == usz[]{0, 2000}, "Array view should hold 2");

	bs.unset(0);
	assert(!bs.get(0), "Get should be false");
	bs[2000] = false;
	assert(!bs.get(2000), "Get should be false");
	assert(bs.cardinality() == 0, "Cardinality should be 0");
}