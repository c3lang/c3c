module test;

struct Foo @align(256)
{
	int x;
}
fn void clone_test() @test
{
	double x;
	void *a = @clone(x);
	free(a);
	(void)@tclone(x);
	Foo y;
	a = @clone_aligned(y);
	assert(((uptr)a) % 256 == 0);
	free_aligned(a);
	a = @tclone(y);
	assert(((uptr)a) % 256 == 0);
	a = @tclone(y);
	assert(((uptr)a) % 256 == 0);
}
