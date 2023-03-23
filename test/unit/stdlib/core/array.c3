module arraytests @test;

fn void! find()
{
	int[3] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)! == 1);
	assert(array::index_of(a, 1)! == 0);
	assert(array::index_of(a, 3)! == 2);
	assert(@catchof(array::index_of(a, 4)) == SearchResult.MISSING);
}

fn void! find_subarray()
{
	int[] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)! == 1);
	assert(array::index_of(a, 1)! == 0);
	assert(array::index_of(a, 3)! == 2);
	assert(@catchof(array::index_of(a, 4)) == SearchResult.MISSING);
}

fn void! concat()
{
	int[3] a = { 1, 2, 3 };
	array::concat(a, a);
	array::concat(a[..], a[..]);
	array::concat(a[:0], a[:0]);
	array::concat(int[2] { 1, 2 }, a[:0]);
	array::concat(a[:0], int[2] { 1, 2 });
	int[] c = array::concat(a[1..2], a);
	assert (c == int[]{ 2, 3, 1, 2, 3 });
}
