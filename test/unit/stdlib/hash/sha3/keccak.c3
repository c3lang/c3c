// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module sha3::keccak_tests @test;

import sha3::tests_common;
import std::hash::sha3::keccak @public;


// There doesn't seem to be any widely available source of Keccak-specific standardized tests.
// fn void keccak_known_vectors()
// {
// }

fn void keccak_empty()
{
	// NOTE: 100 out-length is chosen to demonstrate the XOF usability of keccak.
	test::@check(keccak::hash(224, 100, "")[:28] == x"f71837502ba8e10837bdd8d365adb85591895602fc552b48b7390abd");
	test::@check(keccak::hash(256, 100, "")[:32] == x"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470");
	test::@check(keccak::hash(384, 100, "")[:48] == x"2c23146a63a29acf99e73b88f8c24eaa7dc60aa771780ccc006afbfa8fe2479b2dd2b21362337441ac12b515911957ff");
	test::@check(keccak::hash(512, 100, "")[:64] == x"0eab42de4c3ceb9235fc91acffe746b29c29a8c366b7c60e4e67c466f36a4304c00fa9caf9d87976ba469bcbe06713b435f091ef2769fb160cdab33d3670680e");
}

fn void keccak_simple()
{
	static String[][] expected = {
		{
			x"5c52615361ce4c5469f9d8c90113c7a543a4bf43490782d291cb32d8",
			x"64e604787cbf194841e7b68d7cd28786f6c9a0a3ab9f8b0a0e87cb4387ab0107",
			x"7dd34ccaae92bfc7eb541056d200db23b6bbeefe95be0d2bb43625113361906f0afc701dbef1cfb615bf98b1535a84c1",
			x"8ca32d950873fd2b5b34a7d79c4a294b2fd805abe3261beb04fab61a3b4b75609afd6478aa8d34e03f262d68bb09a2ba9d655e228c96723b2854838a6e613b9d",
		},
		{
			x"162bab64dc3ba594bd3b43fd8abec4aa03b36c2784cac53a58f9b076",
			x"9230175b13981da14d2f3334f321eb78fa0473133f6da3de896feb22fb258936",
			x"c5a708ec2178d8c398461547435e482cee0d85de3d75ddbff54e6606a7e9f994f023a6033b2bf4c516a5f71fc7470d1a",
			x"e55bdca64dfe33f36ae3153c727833f9947d92958073f4dd02e38a82d8acb282b1ee1330a68252a54c6d3d27306508ca765acd45606caeaf51d6bdc459f551f1",
		},
		{
			x"04bb608797b180ec96d1c95f76c98c9bf6b1ceb2e5826e174f8c3ebe",
			x"45cdb357ccfae9c5264099648ba27ff8d9cf4da9129e61b10903886136ce81e9",
			x"17adfa613d47ef5fee0f999fd9b135251e57459f2fff71d7869f2dfd6e3262fe82300c1d828cb5246e626244f98bbaf1",
			x"89516d0f381b7c66f5a54a0fdaa017e3e25fc5ea12f392abb5aefff2d1bfe8b6b1da3ee6a60c7a0f5f576c4bdab0791917f9c116ee1b411cc9bf9ff5b93d640f",
		},
		{
			x"310aee6b30c47350576ac2873fa89fd190cdc488442f3ef654cf23fe",
			x"4d741b6f1eb29cb2a9b9911c82f56fa8d73b04959d3d9d222895df6c0b28aa15",
			x"283990fa9d5fb731d786c5bbee94ea4db4910f18c62c03d173fc0a5e494422e8a0b3da7574dae7fa0baf005e504063b3",
			x"d135bb84d0439dbac432247ee573a23ea7d3c9deb2a968eb31d47c4fb45f1ef4422d6c531b5b9bd6f449ebcc449ea94d0a8f05f62130fda612da53c79659f609",
		},
		{
			x"01607c22de8a4b89d3fc51a62b0a8a42817a3c0f9bb17e88315ffc54",
			x"d2468814452dbca546c9569e132041369ba99dfaebfc83b39c05d706ce0c2397",
			x"5f0935819c68870ffedfd49bc33d5b9a798a1ce6b000a836a1df3219831d99163fe7514135f6405dca13bb3ea235b257",
			x"93a294f9e80c2d0bf26d0e56d333dce9c0523effb8fdba078ef3a6faaa5481d8b86f0ef79af44b646164e8c9bda01fef01bac3a30f818dae3597b6faacabc78a",
		},
	};
	foreach (i, v : tests_common::SIMPLE_VECTORS)
	{
		$foreach $i, $sz : keccak::SECURITY_LEVELS:
			test::@check(keccak::hash($sz, 100, v[..])[:$sz/8] == expected[i][$i][..]);
		$endforeach
	}
}

fn void keccak_interval()
{
	static String[][] expected = {
		{
			x"7cf87d912ee7088d30ec23f8e7100d9319bff090618b439d3fe91308",
			x"3ac225168df54212a25c1c01fd35bebfea408fdac2e31ddd6f80a4bbf9a5f1cb",
			x"85e964c0843a7ee32e6b5889d50e130e6485cffc826a30167d1dc2b3a0cc79cba303501a1eeaba39915f13baab5abacf",
			x"9c46dbec5d03f74352cc4a4da354b4e9796887eeb66ac292617692e765dbe400352559b16229f97b27614b51dbfbbb14613f2c10350435a8feaf53f73ba01c7c",
		},
		{
			x"141b2dd7961364b0ca63dc9d92e32e50d78ad56499b1a599f2fd034a",
			x"c970b103fd7a66de650b6fc134af4bbf8cb7445d7ff7b32150975686ba7a47a6",
			x"2e15df592387e1719d83822ae3136275ba8ab5f3624f3a93a717dcf6875da1df4bffdb7b1dc234f27f0dfd1eae4b704a",
			x"2fd602e39bfa499a2004d4e3fddd6c473b10ce281bc04c85e66064e6ac342fb46ce710ecd88518b18a7ab0694730a6722fd06edc6a4e6f289aae1a1658d03eef",
		},
		{
			x"73b7604360fe89081f178550172045c4214f19ae5ff145d4f3c31c09",
			x"64aa8a28a772ee76c84d6ebde75064fc5282e22a8cf617af1d7e5e08ef9a3248",
			x"ac9dd14c73677928b92a0d36b79073f9a7a78f2c57cb33a49add3006da69822844b6ade4f03e7fb1f64b988ab3717254",
			x"557fbb8e40dc514423f803b4d10c6679593ae5037763f01bc51c760e57515878cace7ffe61e04b0ced896261971c5c2e9d64ebdeaec3f9138f8b3f2bf81f4cde",
		},
		{
			x"21f64e735af04483b601bcc70fbee397f2ce1a81a50290550767e23d",
			x"4c55a8c2dd206fa72622d57dbd05cda14d30779a7c20d8458ecd177dc16895b5",
			x"c8e075be31b6f4a1cff25f3d749b7b7a49ba590aca54f9c8e2d5801323fe85b618242c8d965083e01df9db227b2950db",
			x"fe177696b5b700edb65092578262e813c6fcec999bba104e8a6fc0c0213570e1ad3673b4554aebb60d0d8f57bea6093226210d97b9c60f37f025df8bce0ebea0",
		},
		{
			x"642abc37fb2db70ebffbf7300ddc61788983b1e8131b77d83779e149",
			x"f32e95e2adb0d6bceecf15f0e9ff76e1f1033f6ed18cd6da3edc6f89238072cf",
			x"9b98df14a4e4684989f8d9e71adb40d8ac9812d1539fae1fddb877c443c3e809015f610ac2a319cdd077d76bd8e1fec7",
			x"a7e0a48b9a9344a36531f79cfee05468114e6d3eb5045cc6ac432983ef48ca69ffe05adc7eb4bd196b50972072cc3bf125dc0b45264ff1229e321c92a8e36339",
		},
	};
	static char[257] x = { [0..256] = 'a' };
	// 'a'-array of lengths [1, 65, 129, 193, 257]
	for (usz i = 1, usz j = 0; i <= 257; i += 64, ++j)
	{
		$foreach $i, $sz : keccak::SECURITY_LEVELS:
			test::@check(keccak::hash($sz, 100, x[:i])[:$sz/8] == expected[j][$i]);
		$endforeach
	}
}

fn void keccak_streamed()
{
	static String[] expected = {
		x'c1db269e17762c0cb49f90e8c707a5a07bc8ef916a5be614b9025dcd',
		x'1b3448bae1bf51f295092abf1ab79f0691e519cbd4a3cd9c748ed1c766748609',
		x'b36318ba1ed978d9df23f615958967c0b91d35039f02831dee289055f9fe0a4f6aa1118942b073b1b81f43bc4f2fd4a5',
		x'9ab3c2bd4f3d410e717f8f43ce0842ebb432a5d9bb9e669c95728694e25c3cf3e432a99717187c00ecc89a68dc77fd319c17d085bcc21a7a1104ce1c1e309eda',
	};
	static String s = tests_common::STREAM_VECTOR;
	$foreach $i, $sz : keccak::SECURITY_LEVELS:
	{
		char[$sz / 8] result;
		Keccak{$sz} k @noinit;
		@as_char_view(k)[..] = 0xff;
		k.init();
		k.update(s[:20]); k.update(s[20:10]); k.update(s[30:1]); k.update(s[31..]);
		k.final(result[..]);
		test::@check(result[..] == expected[$i], "%d: Hash mismatch.", $sz);
	}
	$endforeach
}

fn void keccak_large() @if ($feature(SLOW_TESTS))
{
	static String[] expected = {
		x"19f9167be2a04c43abd0ed554788101b9c339031acc8e1468531303f",
		x"fadae6b49f129bbb812be8407b7b2894f34aecf6dbd1f9b0f0c7e9853098fc96",
		x"0c8324e1ebc182822c5e2a086cac07c2fe00e3bce61d01ba8ad6b71780e2dec5fb89e5ae90cb593e57bc6258fdd94e17",
		x"5cf53f2e556be5a624425ede23d0e8b2c7814b4ba0e4e09cbbf3c2fac7056f61e048fc341262875ebc58a5183fea651447124370c1ebf4d6c89bc9a7731063bb",
	};
	char[] bigboi = tests_common::get_bigboi();
	$foreach $i, $sz : keccak::SECURITY_LEVELS:
		test::@check(keccak::hash($sz, $sz / 8, bigboi[..])[..] == expected[$i]);
	$endforeach
}
