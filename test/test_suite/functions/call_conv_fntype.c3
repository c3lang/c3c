alias Foo = fn int(String x, int z) @callconv("veccall");

fn int v(String x, int z) @callconv("veccall")
{
	return 1;
}