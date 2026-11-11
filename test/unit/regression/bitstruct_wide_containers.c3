// Bitstruct fields in 128-bit and byte-array containers, and in an anonymous bitstruct member of a struct.
module bitstruct_wide_containers @test;

bitstruct Wide : uint128
{
	uint lo : 0..31;
	ulong mid : 40..103;
	int top : 104..127;
}

bitstruct Arr : char[16]
{
	int128 big : 4..99;
	ichar small : 100..107;
}

struct Holder
{
	int x;
	bitstruct : ushort
	{
		bool flag : 0;
		char nib : 4..7;
	}
}

fn void uint128_container()
{
	Wide w;
	w.lo = 0xDEADBEEF;
	w.mid = 0x123456789ABCDEF0;
	w.top = -5;
	w.mid += 1;
	w.top++;
	assert(w.lo == 0xDEADBEEF);
	assert(w.mid == 0x123456789ABCDEF1);
	assert(w.top == -4);
}

fn void byte_array_container_signed()
{
	Arr a;
	a.big = -12345678901234567890123;
	a.small = -3;
	a.small--;
	assert(a.big == -12345678901234567890123);
	assert(a.small == -4);
}

fn void anonymous_member()
{
	Holder h;
	h.flag = true;
	h.nib = 9;
	h.nib *= 2;
	assert(h.flag);
	assert(h.nib == 2);
}
