module list_test @test;
import std::collections::bitmap;
import std::collections::list;
import std::io;

def Bitmap = Bitmap(<char>);
def List = List(<usz>);

fn void! set_get()
{
	Bitmap bm;
	bm.tinit();
	assert(bm.len() == 0);

	assert(!bm.get(0));
	bm.set(0);
	assert(bm.get(0));
	assert(bm.cardinality() == 1);

	assert(!bm.get(2000));
	bm[2000] = true;
	assert(bm.get(2000));
	assert(bm.cardinality() == 2);

	List found;
	found.tinit();
	foreach (i, x : bm)
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

	bm.unset(0);
	assert(!bm.get(0));
	bm[2000] = false;
	assert(!bm.get(2000));
	assert(bm.cardinality() == 0);
}