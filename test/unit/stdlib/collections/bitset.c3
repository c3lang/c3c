module bitset_test @test;
import std::collections::bitset;
import std::collections::growablebitset;
import std::collections::list;
import std::io;

def List = List(<usz>);

def BitSet = BitSet(<2048>);

fn void! set_get()
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
	found.tinit();
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

fn void! growable_set_get()
{
	GrowableBitSet bs;
	bs.tinit();
	assert(bs.cardinality() == 0);

	assert(!bs.get(0));
	bs.set(0);
	assert(bs.get(0));
	assert(bs.cardinality() == 1);
	assert(bs.len() == 1);

	assert(!bs.get(2000));
	bs[2000] = true;
	assert(bs.get(2000));
	assert(bs.cardinality() == 2);

	assert(bs.data.len() == 251);
	assert(bs.len() == 2001);

	List found;
	found.tinit();
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