module arraytests @test;

fn void find()
{
	int[3] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == SearchResult.MISSING);
}

fn void find_subarray()
{
	int[] a = { 1, 2, 3 };
	assert(array::index_of(a, 2)!! == 1);
	assert(array::index_of(a, 1)!! == 0);
	assert(array::index_of(a, 3)!! == 2);
	assert(@catch(array::index_of(a, 4)) == SearchResult.MISSING);
}

fn void concat()
{
	int[3] a = { 1, 2, 3 };
	(void)array::concat_new(a, a);
	(void)array::concat_new(a[..], a[..]);
	(void)array::concat_new(a[:0], a[:0]);
	(void)array::concat_new(int[2] { 1, 2 }, a[:0]);
	(void)array::concat_new(a[:0], int[2] { 1, 2 });
	int[] c = array::concat_new(a[1..2], a);
	assert (c == int[]{ 2, 3, 1, 2, 3 });
}
