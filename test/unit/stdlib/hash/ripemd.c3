// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module ripemd_tests @test;

import std::hash::ripemd;
import std::collections;


fn void assert_finalization()
{
	static char[RipeMd{160}.sizeof] empty = {};
	RipeMd{160} ctx @noinit;

	ctx.init();
	test::@check(@as_char_view(ctx) != empty[..], "RIPE-MD cryptographic context is not initialized.");

	ctx.update("beep");
	(void)ctx.final();
	test::@check(@as_char_view(ctx) == empty[..], "RIPE-MD cryptographic context was not implicitly wiped upon finalization:\n\t%s.", @as_char_view(ctx));
}


fn void empties()
{
	test::@check(ripemd::hash{128}("") == x'cdf26213a150dc3ecb610f18f6b38b46');
	test::@check(ripemd::hash{160}("") == x'9c1185a5c5e9fc54612808977ee8f548b2258d31');
	test::@check(ripemd::hash{256}("") == x'02ba4c4e5f8ecd1877fc52d64d30e37a2d9774fb1e5d026380ae0168e3c5522d');
	test::@check(ripemd::hash{320}("") == x'22d65d5661536cdc75c1fdf5c6de7b41b9f27325ebc61e8557177d705a0ec880151c3a32a00899b8');
}


fn void basic()
{
	char[] phrase = "The quick brown fox jumps over the lazy dog";
	test::@check(ripemd::hash{128}(phrase) == x'3fa9b57f053c053fbe2735b2380db596');
	test::@check(ripemd::hash{160}(phrase) == x'37f332f68db77bd9d7edd4969571ad671cf9dd3b');
	test::@check(ripemd::hash{256}(phrase) == x'c3b0c2f764ac6d576a6c430fb61a6f2255b4fa833e094b1ba8c1e29b6353036f');
	test::@check(ripemd::hash{320}(phrase) == x'e7660e67549435c62141e51c9ab1dcc3b1ee9f65c0b3e561ae8f58c5dba3d21997781cd1cc6fbc34');
}

fn void basic_streamed()
{
	static char[][] expected = {
		x'3fa9b57f053c053fbe2735b2380db596',
		x'37f332f68db77bd9d7edd4969571ad671cf9dd3b',
		x'c3b0c2f764ac6d576a6c430fb61a6f2255b4fa833e094b1ba8c1e29b6353036f',
		x'e7660e67549435c62141e51c9ab1dcc3b1ee9f65c0b3e561ae8f58c5dba3d21997781cd1cc6fbc34'
	};
	char[] phrase = "The quick brown fox jumps over the lazy dog";
	$foreach $i, $bits : ripemd::PERMISSIBLE_SIZES_BITS{128}:
	{ // le scope
		RipeMd{$bits} r @noinit;
		r.init();
		r.update(phrase[                   : phrase.len/4 ]);
		r.update(phrase[ 1*(phrase.len/4)  : phrase.len/4 ]);
		r.update(phrase[ 2*(phrase.len/4)  : phrase.len/4 ]);
		r.update(phrase[ 3*(phrase.len/4)  ..             ]);
		test::@check(r.final()[..] == expected[$i]);
	}
	$endforeach
}


fn void large_input()
{
	char[] one_mb = mem::talloc_array(char, 1024*1024);
	one_mb[..] = 0xA5;

	test::@check(ripemd::hash{128}(one_mb) == x'e8b57fa5f8415f7a17043b67509fc182');
	test::@check(ripemd::hash{160}(one_mb) == x'edac5dd2281c86b49b362269636ef1680283d291');
	test::@check(ripemd::hash{256}(one_mb) == x'c635c1763d2766e617f095fb3bb65ed3b533fb48a7ac127916a2f2ba1127d3cf');
	test::@check(ripemd::hash{320}(one_mb) == x'3173e524ea770b420c986974b486ea18d4558db55000e6db67784fe5fc091fe5fcc8bc80b7753c34');
}

fn void large_input_streamed()
{
	static char[][] expected = {
		x'e8b57fa5f8415f7a17043b67509fc182',
		x'edac5dd2281c86b49b362269636ef1680283d291',
		x'c635c1763d2766e617f095fb3bb65ed3b533fb48a7ac127916a2f2ba1127d3cf',
		x'3173e524ea770b420c986974b486ea18d4558db55000e6db67784fe5fc091fe5fcc8bc80b7753c34'
	};
	char[] one_mb = mem::talloc_array(char, 1024*1024);
	one_mb[..] = 0xA5;
	$foreach $i, $bits : ripemd::PERMISSIBLE_SIZES_BITS{128}:
	{
		RipeMd{$bits} r @noinit;
		r.init();
		r.update(one_mb[                   : one_mb.len/4 ]);
		r.update(one_mb[ 1*(one_mb.len/4)  : one_mb.len/4 ]);
		r.update(one_mb[ 2*(one_mb.len/4)  : one_mb.len/4 ]);
		r.update(one_mb[ 3*(one_mb.len/4)  ..             ]);
		test::@check(r.final()[..] == expected[$i]);
	}
	$endforeach
}

fn void large_input_streamed_per_byte()
{
	static char[][] expected = {
		x'e8b57fa5f8415f7a17043b67509fc182',
		x'edac5dd2281c86b49b362269636ef1680283d291',
		x'c635c1763d2766e617f095fb3bb65ed3b533fb48a7ac127916a2f2ba1127d3cf',
		x'3173e524ea770b420c986974b486ea18d4558db55000e6db67784fe5fc091fe5fcc8bc80b7753c34'
	};
	char[] one_mb = mem::talloc_array(char, 1024*1024);
	one_mb[..] = 0xA5;
	$foreach $i, $bits : ripemd::PERMISSIBLE_SIZES_BITS{128}:
	{
		RipeMd{$bits} r @noinit;
		r.init();
		foreach (&c : one_mb) r.update(c[:1]);
		test::@check(r.final()[..] == expected[$i]);
	}
	$endforeach
}


fn void walk_lengths()
{
	foreach (arr : CALCULATED_HASHES)
	{
		test::@check(ripemd::hash{128}(arr[0])[..] == arr[1]);
		test::@check(ripemd::hash{160}(arr[0])[..] == arr[2]);
		test::@check(ripemd::hash{256}(arr[0])[..] == arr[3]);
		test::@check(ripemd::hash{320}(arr[0])[..] == arr[4]);
	}
}

fn void walk_lengths_streamed()
{
	foreach (arr : CALCULATED_HASHES)
	{
		$foreach $i, $bits : ripemd::PERMISSIBLE_SIZES_BITS{128}:
		{
			RipeMd{$bits} r @noinit;
			r.init();
			r.update(arr[0][                   : arr[0].len/4 ]);
			r.update(arr[0][ 1*(arr[0].len/4)  : arr[0].len/4 ]);
			r.update(arr[0][ 2*(arr[0].len/4)  : arr[0].len/4 ]);
			r.update(arr[0][ 3*(arr[0].len/4)  ..             ]);
			test::@check(r.final()[..] == arr[$i + 1]);
		}
		$endforeach
	}
}

const char[][5][] CALCULATED_HASHES = {
	// various <1 block
	{ "a", x'86be7afa339d0fc7cfc785e72f578d33', x'0bdc9d2d256b3ee9daae347be6f4dc835a467ffe', x'f9333e45d857f5d90a91bab70a1eba0cfb1be4b0783c9acfcd883a9134692925', x'ce78850638f92658a5a585097579926dda667a5716562cfcf6fbe77f63542f99b04705d6970dff5d' },
	{ "abcde", x'a0a954be2a779bfb2129b72110c5782d', x'973398b6e6c6cfa6b5e6a5173f195ce3274bf828', x'81d8b58a3110a9139b4ddeccb031409e8af023067cf4c6f0b701dab9ecc0eb4e', x'a94dc1bc825db64e97718305ce36bfef32cc5410a630999678bcd89cc38c424269012ec8c5a95830' },
	{ "abcdeabcd", x'635cd0a53e0dd5165cac8ddbee7652cd', x'807e17a70c332849b16781f6ee7922f5e7db63f8', x'31b029446e713234e05d17b7b1007c97ed26b6c536b28c054db102e02f34b12b', x'46d38e1f11d62ca12e0597a975b072bf2e2c6f11bea0e4b304d4c2da54fb06b21282d7140f312089' },
	{ "abcdeabcdabcdeab", x'23465727108702623665cf26343522b9', x'8547fee8ca4574daae85bdccb86ef55c5fe2846f', x'63f27e77a68dc95be6bad32d16b7760448571e5042dade5a1238db9fa693fea3', x'bfd1594d518db7a40a3dede4a45201db1051ce8a8d0261f83c1725ffe4baf0a8ace7f069a562d79f' },
	{ "abcdeabcdabcdeabc", x'b4648632cbb6a15b81f512a33b723473', x'9b7c05456e9168db1d00bb51e16370a3593b9465', x'b6f9371183c1231fa28eb02b0b005848bf9490023811cdc6d724e7c2e722ce85', x'06dfd96b1550647833dbb2e29a5ae470e5125d88e4876b9bb5ba10302fbd4d85fbe789bf41ba569a' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdea", x'1e22bb88890fab194b3d6bd8b9def772', x'4ecc7066704497cf3dbb8dfcff22bcbc4892f642', x'2bacfe18130dec744e97499a49f02ba28e811c931db3f13f7b088d0af409a8fe', x'cdf8a2dda089fdeb5dcad3eb379470bbdbb4ad730195d791b36f0979d2c45190c64a79f097efe0bf' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeab", x'db41ba3b4ff1d41055374c929a89e0e5', x'8e80b382727b7ea13ad7698fc07fa1c58e1ad451', x'779ccb4ea231bcbe0285f03202a332ac46e86e1434d0aa6e996e71c554902933', x'55aaccefb7e027aed5305a752576e7deb8afd3c6317d1a47fd9545b02c70405b12a4d69a14f881c0' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcde", x'00a8da1bdcca00dc7538ac890266acb8', x'efce1e52dd937bcaf04aeea66db30d3a36af8d9d', x'5e2dd99cec7e02907910f14526e4aea39b5d1d9bc148ebbea685c26dfd965619', x'8c135e36adce8e3869cf1bfd671fa908585cfa415a345f10c1e188b9622d5400648a6fd6e7be9296' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcd", x'c09e2bbaf5d230c58227c77a59e13c10', x'738c221b1c1a63f29fc1616389395825493f6053', x'ec17e9420c1268dc293591b3217b11d9bce2a57fcfcb9efac3e328079684e348', x'5ace0ffaa6b20e277794a0b38de747f6f6b69a1b44141a9dc835eeb0cd8802370cc3f1cd72638e1d' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcde", x'14e8d04a7f0841155958c9c0322234ac', x'5c459f85203088a16df8eb0535b625c847a3a978', x'64b720261a622380a595707ebb14f612be3aff551454886736427639a71912e0', x'227d94ced760cfdcfc80554cbd32d77058cd84b2ba970a0c060a24725dc5c8de89eed859b0df1e4e' },
	// near one-block boundary
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdea", x'f4509de22563bd3b33f3baf5a7a8ae1a', x'510cbbfc90525eb4215ea43d667ecced08121f46', x'681eca70274324398280d03750b78d90d1ca0d3242687d64afe6c12f87e59f2a', x'20e2e1b19b523cddd421fca650608b12bc0ba194b2279d1992622ee4e98a29e76db7acb9f8b8fb34' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeab", x'ba653f72f0de768b94bdaa2d8b3a4780', x'e114478d20e436c0e09222750c1606235308e25a', x'176f0f5f132f0f5aee2be93658afb7b82ce8ed5918016cdf778acad368d45ade', x'f41eee99b9a7cdee313ba7852315a4ad49fde956e28a5858b4e0590ef437ec427c6b71c688e07b32' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabc", x'644008f8e0fcd0a97b7840b7c12691c8', x'fd47e805822cb81c0efb8da28c51420c0e33b5a9', x'25ca9b0b822a0e2ae24ff98b709d2c88966364c9c19975c3cddc155b94787606', x'2ba0071efd27440518674432ad6d14c0cf0b53631123eae655aa1c3d08b8fecb4693ce2f98494f3b' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcd", x'e7033b021f73230ebb634edc90fb59ae', x'59b27afdf089f7a20e09614468da32430358f5cd', x'515c7c8184253313cdb98a5341dbeeeeb889f153d867bfc8cffd667f3596aed8', x'ef03ee21dda0c64ea9a28ddc4bc935a486668b237bba83b89a512e4e7d2a08a00b5494c4ce74425b' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcde", x'40b12f14498a838e671bf63070ea61e0', x'ea5d980bc60a4dbc5724773da49bb8d4a5e922e3', x'4fe1f00eba7869455852dbabcbd56566a8d24177fb66d67696d3c27aaaefe3a0', x'c60fe39e9ab75f960319cc2f4b7af805bb635e53082a77748234a1ef86323ea01891a13d70f28c01' },
	// near two-block boundary
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcde",
		x'ae8cc9d8144b9e57de33b36c3ca6185f', x'2465de27e6e0255ffc1e28a6d4731ab59ce8e43b', x'a4a8d0f209f496a7f9008dec19b72c138887dac2a3b72f4bb25632b18ca0d73e', x'425ecf1a6e59b76078478ebc5a4ecf90a27742cfea19626f0f7785e2c62d8b5fddf03c5b213c6f36' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdea",
		x'5cc7dbc0b33fcef3927d13584459e925', x'7ea3aabcbb9331b5b2374cb6cb663d10d377316d', x'11689e37b81c568a3be70678e4697a0f00c76a55642d57897d0d6e470a289352', x'820979996008b7bfec51e258a0152336c875917decf1ab5ab0ece063cf28cf4421977b70138ce92a' },
	{ "abcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdeabcdabcdeabcabcdeabcdabcdeabcdeabcdeabcdeabcdeabcdeabcdeab",
		x'7184b353ae47499bdea3c71f6d4e9233', x'f1ab66b1cfb5a1ce6fedae1d6165e8e5629be43d', x'70edf63d3700f4b041c8b5737fb93ad329dab5711c8bd57beae7f696e1fdef45', x'10eeb15dd5397626ac7a2bd6dc07e19cdfb739acf4e134f928d6f087804cb767293a6f7ccb6705f2' },
};
