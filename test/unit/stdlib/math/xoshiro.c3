module xoshiro_tests @private;

macro void @fill_seeds_32(#s, uint seed)
{
	static uint next_chunk;
	foreach (&_s : #s.state)
	{
		if (!next_chunk)
		{
			ulong n = #s.next_long();
			*_s = (uint)(n >> 32);
			next_chunk = (uint)n;
		}
		else
		{
			*_s = next_chunk;
			next_chunk = 0;
		}
	}
}

macro void @fill_seeds_64(#s, ulong seed)
{
	foreach (&_s : #s.state) *_s = #s.next_long();
}


module xoshiro_tests @test;

import std::math::random;

fn void xoshiro128pp()
{
	// Original Xoshiro128pp test
	Xoshiro{128, XoshiroOperation.PLUS_PLUS} x = { { 1, 2, 3, 4 } };
	test::eq(0x281, x.next_int());
	test::eq(0x180387, x.next_int());
	test::eq(0xc0183387, x.next_int());
	x = { { ~1u, ~2u, ~3u, ~4u } };
	test::eq(0xfffffcfd, x.next_int());
	test::eq(0x17fbf8, x.next_int());
	test::eq(0x40183386, x.next_int());
	// =================================

	x = { { 3871345877, 3193868661, 420689323, 819476962 } };
	const uint[] EXPECTED = {
		3001247840, 771162000, 3060113903, 1554348208, 4245951544, 3539921958, 904526743, 1457038137, 3977941375, 3298493474,
		3026273907, 3040247804, 530699525, 676727785, 1104359093, 509038229, 4211111063, 984989411, 175609725, 2162096349,
		221506958, 1334887438, 1006604779, 3409180948, 1532363471, 941739182, 3038056258, 424034730, 1463839907, 1328683967,
		2973638986, 3761413568, 3643040988, 2686799487, 928380834, 2871848520, 324010704, 472260860, 173222120, 2249131766,
		1059166462, 3641710570, 3500475769, 3273457406, 3821595801, 2736000177, 521982834, 1439749432, 2228166104, 3622210891,
	};
	foreach (e : EXPECTED) test::eq(x.next_int(), e);
}
