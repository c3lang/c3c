fn void main()
{
	$assert(-0x80000000 < 0);
	$assert(-0x80000000 != 0x80000000);
	$assert(-0x8000000000000000 < 0);
	$assert(-0x8000000000000000 != 0x8000000000000000);
	$assert($sizeof(0x7fffffff) == 4);
	$assert($sizeof(0x80000000) == 4);
	$assert($sizeof(2147483648) == 8);
	$assert(-0x1 < 0);
	$assert(-01 < 0);
	$assert(-0o1 < 0);
	$assert(-0b1 < 0);
	$assert(@typeid(-2147483648) == int.typeid);
	$assert(@typeid(0x1) == uint.typeid);
	$assert(@typeid(0x8000_0000_0000_0000) == ulong.typeid);
	$assert(@typeid(-0x7FFF_0000_0000_0000) == long.typeid);
	int128 x = (int128)-(0x8000_0000_0000_0000_0000_0000_0000_0000 - 1u);
	assert(-170141183460469231731687303715884105727 == x);
	$assert(@typeid(-170141183460469231731687303715884105728) == int128.typeid);
	$assert(@typeid(170141183460469231731687303715884105727) == int128.typeid);
	$assert(@typeid(170141183460469231731687303715884105728U) == uint128.typeid);
}
