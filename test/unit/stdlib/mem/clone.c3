module test;

fn void clone_test() @test
{
	double x;
	void *a = @clone(x);
	free(a);
	(void)@tclone(x);
	a = @clone_aligned(x);
	free_aligned(a);
	(void)@tclone_aligned(x);
}
