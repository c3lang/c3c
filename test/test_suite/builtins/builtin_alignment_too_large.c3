// #target: linux-x64
module std::test;
fn void unaligned_store(int* p)
{
	$$unaligned_store(p, 0, 2147483648, false); // #error: Alignment must be less or equal to 268435456
}

fn void unaligned_store_null()
{
	$$unaligned_store(null, 0, 536870912, false); // #error: Alignment must be less or equal to 268435456
}

fn void unaligned_load(int* p)
{
	int x = $$unaligned_load(p, (usz)1 << 40, false); // #error: Alignment must be less or equal to 268435456
}

fn void memcpy_align(int* p)
{
	$$memcpy(p, p, (usz)4, false, (usz)1 << 31, (usz)4); // #error: Alignment must be less or equal to 268435456
}

fn void valid(int* p)
{
	$$unaligned_store(p, 0, 268435456, false);
	int x = $$unaligned_load(p, 1, false);
	$$memcpy(p, p, (usz)4, false, (usz)4, (usz)4);
}
