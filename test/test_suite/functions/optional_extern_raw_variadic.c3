// An optional extern C-variadic function: the value pointer comes before the named parameters.
extern fn int? ext_sum(int n, ...) @cname("ext_sum");

fn void? test()
{
	int x = ext_sum(3, 1, 2, 3)!;
	(void)ext_sum(0);
}
