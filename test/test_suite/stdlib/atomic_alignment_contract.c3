import std::atomic;

fn void fetch_add_too_large()
{
	int y;
	atomic::fetch_add(&y, 1, $alignment: 1 << 30); // #error: The alignment must be 0 or a power of two
}

fn void fetch_max_too_large()
{
	int y;
	atomic::fetch_max(&y, 1, $alignment: 1 << 30); // #error: The alignment must be 0 or a power of two
}

fn void fetch_min_not_power_of_two()
{
	int y;
	atomic::fetch_min(&y, 1, $alignment: 3); // #error: The alignment must be 0 or a power of two
}

fn void load_too_large()
{
	int y;
	mem::load(&y, 1 << 30); // #error: The alignment must be a valid power of two
}

fn void valid()
{
	int y;
	atomic::fetch_add(&y, 1, $alignment: 4);
	atomic::fetch_max(&y, 1, $alignment: 4);
	atomic::fetch_min(&y, 1);
	int z = mem::load(&y, 4);
}
