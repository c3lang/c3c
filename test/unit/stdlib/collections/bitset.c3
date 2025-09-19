module bitset_test @test;
import std::collections::bitset;
import std::collections::growablebitset;
import std::collections::list;
import std::io;

alias List2 = List {usz};

alias BitSet2 = BitSet {2048};

fn void bit_ops_assign()
{
	BitSet2 bs;
	BitSet2 bs2;
	bs.set(4);
	bs.set(6);
	bs2.set(4);
	bs2.set(8);
	BitSet2 bs3 = bs;
	bs3 ^= bs2;
	assert(!bs3.get(4));
	assert(!bs3.get(5));
	assert(bs3.get(6));
	assert(bs3.get(8));
	BitSet2 bs4 = bs;
	bs4 |= bs2;
	assert(bs4.get(4));
	assert(!bs4.get(5));
	assert(bs4.get(6));
	assert(bs4.get(8));
	BitSet2 bs5 = bs;
	bs5 &= bs2;
	assert(bs5.get(4));
	assert(!bs5.get(5));
	assert(!bs5.get(6));
	assert(!bs5.get(8));
}

fn void bit_ops()
{
	BitSet2 bs;
	BitSet2 bs2;
	bs.set(4);
	bs.set(6);
	bs2.set(4);
	bs2.set(8);
	BitSet2 bs3 = bs ^ bs2;
	assert(!bs3.get(4));
	assert(!bs3.get(5));
	assert(bs3.get(6));
	assert(bs3.get(8));
	BitSet2 bs4 = bs | bs2;
	assert(bs4.get(4));
	assert(!bs4.get(5));
	assert(bs4.get(6));
	assert(bs4.get(8));
	BitSet2 bs5 = bs & bs2;
	assert(bs5.get(4));
	assert(!bs5.get(5));
	assert(!bs5.get(6));
	assert(!bs5.get(8));
}
fn void set_get()
{
	BitSet2 bs;
	assert(bs.cardinality() == 0);

	assert(!bs.get(0));
	bs.set(0);
	assert(bs.get(0));
	assert(bs.cardinality() == 1);

	assert(!bs.get(2000));
	bs[2000] = true;
	assert(bs.get(2000));
	assert(bs.cardinality() == 2);

	List2 found;
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
	assert(found.array_view() == (usz[]){0, 2000});

	bs.unset(0);
	assert(!bs.get(0));
	bs[2000] = false;
	assert(!bs.get(2000));
	assert(bs.cardinality() == 0);
}

alias GrowableBitSetChar = GrowableBitSet{char};
fn void growable_set_get()
{
	GrowableBitSetChar bs;
	bs.tinit();
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

	List2 found;
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
	assert(found.array_view() == (usz[]){0, 2000}, "Array view should hold 2");

	bs.unset(0);
	assert(!bs.get(0), "Get should be false");
	bs[2000] = false;
	assert(!bs.get(2000), "Get should be false");
	assert(bs.cardinality() == 0, "Cardinality should be 0");
}