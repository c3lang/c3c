// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::siphash_tests;
import std::math;

char[64] plaintext = math::iota(char[64]);


module std::hash::siphash_tests @test;

import std::hash;

const uint128 DEFAULT_TEST_KEY = 0x0f0e_0d0c_0b0a_0908_0706_0504_0302_0100;

fn void sanity_check_default_test_key()
	=> test::@check(((char*)&DEFAULT_TEST_KEY)[:16] == { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 }, "Bad key value as char[].");


fn void streamed_plaintext_64()
{
	ulong expected = 0x958a324ceb064572;

	SipHash24 s;

	s.init(DEFAULT_TEST_KEY);

	s.update(plaintext[:5]);
	s.update(plaintext[5:25]);
	s.update(plaintext[30..^2]);   // because the end of the for-loop doesn't get to the full 64 chars, it's 63

	ulong hashval = s.final();

	test::@check(hashval == expected, "Expected streamed hash mismatch [%x expected // %x actual].", expected, hashval);
}


fn void streamed_plaintext_128()
{
	uint128 expected = 0x7cbd3f979a063e504a83502f77d15051;

	SipHash24_128 s;

	s.init(DEFAULT_TEST_KEY);

	s.update(plaintext[:1]);
	s.update(plaintext[1:2]);
	s.update(plaintext[3:12]);
	s.update(plaintext[15..^2]);   // because the end of the for-loop doesn't get to the full 64 chars, it's 63

	uint128 hashval = s.final();

	test::@check(hashval == expected, "Expected streamed hash mismatch [%x expected // %x actual].", expected, hashval);
}


// These siphash-2,4 vectors are from the great and wise sages themselves:
//   https://github.com/veorq/SipHash/blob/master/vectors.h
const ulong[64] VECTORS_24 = {
	0x726fdb47dd0e0e31, 0x74f839c593dc67fd, 0x0d6c8009d9a94f5a, 0x85676696d7fb7e2d,
	0xcf2794e0277187b7, 0x18765564cd99a68d, 0xcbc9466e58fee3ce, 0xab0200f58b01d137,
	0x93f5f5799a932462, 0x9e0082df0ba9e4b0, 0x7a5dbbc594ddb9f3, 0xf4b32f46226bada7,
	0x751e8fbc860ee5fb, 0x14ea5627c0843d90, 0xf723ca908e7af2ee, 0xa129ca6149be45e5,
	0x3f2acc7f57c29bdb, 0x699ae9f52cbe4794, 0x4bc1b3f0968dd39c, 0xbb6dc91da77961bd,
	0xbed65cf21aa2ee98, 0xd0f2cbb02e3b67c7, 0x93536795e3a33e88, 0xa80c038ccd5ccec8,
	0xb8ad50c6f649af94, 0xbce192de8a85b8ea, 0x17d835b85bbb15f3, 0x2f2e6163076bcfad,
	0xde4daaaca71dc9a5, 0xa6a2506687956571, 0xad87a3535c49ef28, 0x32d892fad841c342,
	0x7127512f72f27cce, 0xa7f32346f95978e3, 0x12e0b01abb051238, 0x15e034d40fa197ae,
	0x314dffbe0815a3b4, 0x027990f029623981, 0xcadcd4e59ef40c4d, 0x9abfd8766a33735c,
	0x0e3ea96b5304a7d0, 0xad0c42d6fc585992, 0x187306c89bc215a9, 0xd4a60abcf3792b95,
	0xf935451de4f21df2, 0xa9538f0419755787, 0xdb9acddff56ca510, 0xd06c98cd5c0975eb,
	0xe612a3cb9ecba951, 0xc766e62cfcadaf96, 0xee64435a9752fe72, 0xa192d576b245165a,
	0x0a8787bf8ecb74b2, 0x81b3e73d20b49b6f, 0x7fa8220ba3b2ecea, 0x245731c13ca42499,
	0xb78dbfaf3a8d83bd, 0xea1ad565322a1a0b, 0x60e61c23a3795013, 0x6606d7e446282b93,
	0x6ca4ecb15c5f91e1, 0x9f626da15c9625f3, 0xe51b38608ef25f57, 0x958a324ceb064572,
};

fn void siphash24_vectors()
{
	for (usz i = 0; i < VECTORS_24.len; ++i)
	{
		ulong hashval = siphash24::hash(plaintext[:i], DEFAULT_TEST_KEY);

		test::@check(hashval == VECTORS_24[i], "Expected hash mismatch [%x expected // %x actual].", VECTORS_24[i], hashval);
	}
}


const ulong[64] VECTORS_48 = {
	0xc879052b9938da41, 0xc85914f95295b851, 0x33c3ddbef0163792, 0x05c147657dd4466a,
	0x48fac14a2b5938c2, 0xe14752cfd9d7c2f6, 0x8e5535c834bcb66b, 0x4efdbe5a713fd747,
	0x50db2f079c8bb520, 0x5312e15ef39a3136, 0x8f848d0adbd0a948, 0x810a0436603969cc,
	0x6197a77a53686d4b, 0x6950c9f2e9963729, 0x689a62a7ea1b4388, 0x83d389d57da9a6e0,
	0x70acb28053f59c55, 0x4e79e37a11c5b7d5, 0x2b10ad3446453c5a, 0xbc3d5aa3af80a4c0,
	0xc84b28e50927c278, 0x9dd6eb0d467026ef, 0xd884d0a986ef76d9, 0xe8d0ea191881d9e3,
	0x16ecea3eb53c3389, 0xc64973645f6c1531, 0xa432763535ce4ca5, 0xfed2a7c025895d06,
	0x8b3a1a2282aabb2b, 0x707b0964cefb0b87, 0x8bee9564f9e0d840, 0x12dffa0bf4a7fc79,
	0xd29e762ff2fb0b00, 0xfa22e5f891556840, 0x0d9d14d874fee62b, 0xed60750b0e2f7eba,
	0x97e1a7ed84e3e902, 0xb6632795620ae8c4, 0xd36d5c5dc6ed2783, 0xc02fa464d164fc79,
	0x4e61fccb11754a15, 0x6fe6a0ec7c8d148b, 0xfa03c454b669eedf, 0xc9b77b69a6368fc5,
	0x2131c6059cbec5a6, 0x3189cdfb59878ab5, 0x25c4cc04673a68d7, 0x8d44a2e5e1e66acb,
	0x73513a3a5b69266e, 0x4aac339fcf077178, 0x84747bd9da907516, 0x06f36bf01e686b00,
	0xa6cfef6602309b1c, 0x4bb3b0d1882f8d28, 0xfe6bf5acbd0611e0, 0x28036e5b0e1f10c0,
	0x0a1c1b5b4591a7c3, 0x0f3a0b9ee1af0757, 0x4f5953fe29725ae6, 0x4caf1aabb99d2f00,
	0x0606c14450cb2859, 0x2173857b960138d5, 0xcc99091a4f36db05, 0x23de0355bc8477e6,
};

fn void siphash48_vectors()
{
	for (usz i = 0; i < VECTORS_48.len; ++i)
	{
		ulong hashval = siphash48::hash(plaintext[:i], DEFAULT_TEST_KEY);

		test::@check(hashval == VECTORS_48[i], "Expected hash mismatch [%x expected // %x actual].", VECTORS_48[i], hashval);
	}
}


// Yet again, these test vectors are prized heirloom values handed down through the generations:
//   https://github.com/veorq/SipHash/blob/master/vectors.h
const uint128[64] VECTORS_24_128 = {
	0x930255c71472f66de6a825ba047f81a3, 0x45fc229b1159763444af996bd8c187da, 0xe4ff0af6de8ba3fcc75da4a48d227781, 0x51ed8529b0b6335f4ea967520cb6709c,
	0x7955cd7b7c6e0f7daf8f9c2dc16481f8, 0x27960e69077a5254886f778059876813, 0x5ea1d78f30a05e481386208b33caee14, 0x3982f01fa64ab8c053c1dbd8beebf1a1,
	0xb49714f364e2830f61f55862baa9623b, 0xed716dbb028b7fc4abbad90a06994426, 0xbafbd0f3d34754c956691478c30d1100, 0x18dce5816fdcb4a277666b3868c55101,
	0x25c13285f64d638258f35e9066b226d6, 0xf752b9c44f9329d0108bc0e947e26998, 0x024949e45f48c77e9cded766aceffc31, 0xd9c3cf970fec087e11a8b03399e99354,
	0x77052385bf1533fdbb54b067caa4e26e, 0x4077e47ac466c05498b88d73e8063d47, 0x23f7aefe81a44d298548bf23e4e526a4, 0xb12e51528920d574b0fa65cf31770178,
	0xeb3938e8a544933e7390223f83fc259e, 0x121d073ecd14228a215a52be5a498e56, 0xae0aff8e52109c469a6bd15245b5294a, 0x1c69bf9a9ae28ccfe0f5a9d5dd84d1c9,
	0xad32618a178a2a88d850bd78ae79b42d, 0x6f8f8dcbeab951507b445e2d045fce8e, 0x661f147886e0ae7ee807c3b3b4530b9c, 0x94eb9e122febd3bfe4eaa669af48f2ab,
	0xf4ae587302f335b9884b576816da6406, 0xb76a7c463cfdd40ce97d33bfc49d4baa, 0x87226d68d4d71a2bde6baf1f477f5cea, 0x353dc4524fde2317fcfa233218b03929,
	0x68eb4665559d3e363efcea5eca56397c, 0xcfffa94e5f9db6b6321cf0467107c677, 0xde549b30f1f02509df7e84b86c98a637, 0xc88c3c922e1a2407f9a8a99de6f005a7,
	0x11674f90ed769e1e4648c4291f7dc43d, 0x2b69d3c551473c0d1a0efce601bf620d, 0xb5e7be4b085efde49e667cca8b46038c, 0xd92bd2d0e5cc73449c2caf3bb95b8a52,
	0xd83b91c6c80cae97ad5dc9951e306adf, 0xdbb6705e289135e7397f852c90891180, 0x5b0ccacc34ae5036bb31c2c96a3417e6, 0x89df5aecdc211840aa21b7ef3734d927,
	0x4273cc66b1c9b1d8785e9ced9d7d2389, 0x4cb150a294fa8911657d5ebf91806d4a, 0x022949cf3d0efc3f89aee75560f9330e, 0x1b1563dc4bd8c88ed1190b722b431ce6,
	0x169b2608a6559037cf82f749f5aee5f7, 0x03641a20adf237a84fa5b7d00f038d43, 0x3f4286f2270d7e24e304bf4feed390a5, 0x38f5f9ae7cd35cb1c493fe72a1c1e25f,
	0x7c013a8bd03d13b26eb306bd5c32972c, 0x9ed32a009f65f09f94ca6b7a2214c892, 0x871d91d64108d5fb8c32d80b1150e8dc, 0xda832592b52be3481279dac78449f167,
	0x362a1da96f16947ee94ed572cff23819, 0x8e6904163024620ffe49ed46961e4874, 0x1d8a3d58d0386400d8d6a998dea5fc57, 0x595357d9743676d4be1cdcef1cdeec9f,
	0x40e772d8cb73ca6653f128eb000c04e3, 0x7a0f6793591ca9ccfe1d836a9a009776, 0xbd5947f0a447d505a067f52123545358, 0x7cbd3f979a063e504a83502f77d15051,
};

fn void siphash24_128_vectors()
{
	for (usz i = 0; i < VECTORS_24_128.len; ++i)
	{
		uint128 hashval = siphash24_128::hash(plaintext[:i], DEFAULT_TEST_KEY);

		test::@check(hashval == VECTORS_24_128[i], "Expected hash mismatch [%x expected // %x actual].", VECTORS_24_128[i], hashval);
	}
}


const uint128[64] VECTORS_48_128 = {
	0x6c0aa78354e8eccfe904a96d58ce641f, 0x80a2c791a77cf26a47794cefa85d3447, 0xa42182185f817322c62dca96a35f49e1, 0x027605817fb69c5a834ec54a8473a2c7,
	0xef6d651fe0c8952a4ece3ef4bb521f54, 0x5dbe12bf0c994f241548f30dd43b9717, 0xfff5e108c9567db1cd8032560d360b6b, 0xe0eed2ff548b6b72c2f14b183be100ed,
	0x63e0caaf235a4a36f5edf98f1346d9a7, 0xe80bfbe449559a8ba3ec5c54b714739e, 0x7512b99a885be6ae68d18984c6626c58, 0x997a2e2f1e84abc447d1a34ca65271e6,
	0xaf16a8ee83089e3e2ee58d90ea7a1c7f, 0x344aefb50d33353f9233b792bf7a82de, 0xf97d4c35e28e6737c59a370f64637559, 0xcb6290dcfaf7783d593a453a30034d28,
	0x9cad4e8dabe5fdc0b7637f59a2c74a91, 0xcbc387ce953b453aee55a44ba415510d, 0x1479ce845a88378394d8f10b9d939b54, 0x6cb4728c5713fbd2eb8a34cd6997176c,
	0x8b40862fee002a68e057c938c136d0aa, 0x263ad74eff4490eebf70b62fc4eeb121, 0xeb143f4617c9534637ed9729d6a19305, 0x4ee25786c6fff1a01ef9197762313d11,
	0xd6c452c2f014730edd6ae02df74c39b3, 0xd28963cde46b45e241c3359dda982a92, 0x4d8db2cb50eadca24ab357f730626b59, 0x48ee596575df8406807e5bd597e44ec2,
	0x778101dce49d2b5ed41e6836a1b69c5e, 0xb278819dfeed337904d35686ca39fabf, 0xa3ae34813a807a355a79d0a118942218, 0x6eb0771a57697b732e4e4753ff963e4a,
	0x5094f1632f572f2e847237d0f9f0d5fe, 0x6f44bcc629660cc46342f9c186583339, 0x9fbf8b1997462c8bb01087b33bf9a5ee, 0x3ad46661061243370e724f70b6c76e80,
	0xa2b24760aeeeaf9db439c9f09ded696e, 0x38d72f7f1730a294c7f9b698f27bc793, 0xe60ce64c2a3affda75a82a8cd99cadff, 0xe310f55776ef64cdcd934af9fd2f994d,
	0x9bce1bbe96e086a11ea1e0244e627032, 0x77313409c4c7ff1f51ff4ecbe0bbe831, 0x5bab1670a0128a6407d99a87057de1cb, 0xfc0a6a36468bd2d5e28be97043d44888,
	0x0c5e095465cfb50ca9761042b2d1ffb7, 0x0c05d11ffe84d8bbf6a823d56c666b6a, 0x24621330bd8cf105c8f5fb50838afea8, 0xb5627ed594963aebf23682ee7a11e7cc,
	0x049ff6feba90306b0cb728fce4f0253a, 0x4add3144e4f806818bc49f7426e6053f, 0x5b3477dcfad5ff0a5c167276f9796876, 0x0e2fdda19484c2c98b596cb65aa07143,
	0x6602e0d6efb6221fbaf1a5a2d35bf865, 0x1166af8a229f6acaef224be5da61cf76, 0xda84a49dab9053889fa2db9fe3c2dc6c, 0x5aaa2f7b61e2e4d8b2673bcceaaceee1,
	0x6d5aabd52eb0d678170fe14c6f9fd20b, 0x07a6655b834587e07c26526a159f18ad, 0x8f211ce3996bec3dd466257271996b0f, 0x741a9e3d6f9cf3d366f43d4ffac8a4a1,
	0x1df99e2da7d8a6c11fc2f08cb83d1a3b, 0xe52e698a5a63c3562800c0ef026848d1, 0x3c1c2cc2314956fd9919ae7c8f5fa1ee, 0xa5d46b90ee61792093dbc42863aef563,
};

fn void siphash48_128_vectors()
{
	for (usz i = 0; i < VECTORS_48_128.len; ++i)
	{
		uint128 hashval = siphash48_128::hash(plaintext[:i], DEFAULT_TEST_KEY);

		test::@check(hashval == VECTORS_48_128[i], "Expected hash mismatch [%x expected // %x actual].", VECTORS_48_128[i], hashval);
	}
}


alias SecHash = SipHash { uint128, 256, 512 };
alias sec_hash = siphash::hash { uint128, 256, 512 };

fn void test_custom_sip()
{
	uint128 key = 0x8a52f8467c2fafa9ab4d7396d6fe01a3;
	char[] data =
		"As you can see, the hash rounds are flexible and so is the return type, between a 64-bit and 128-bit key."
		" Output values are simple, quick, and deterministic."
			.tcopy();

	uint128 expected = 0x47de68b601e964b343c29358f864cc40;
	uint128 actual = sec_hash(data, key);

	test::@check(actual == expected, "Expected hash mismatch [%x expected // %x actual].", expected, actual);

	// ---------- Slight data change.
	data[0] = 'a';
	expected = 0x15fa9037e63089732d86ab5bab3938e1;
	actual = sec_hash(data, key);

	test::@check(actual == expected, "Expected hash mismatch [%x expected // %x actual].", expected, actual);

	// ---------- Slight key change; streamed.
	key -= 1;
	expected = 0xceeaca813e87ad83dde110a402eac1ef;
	SecHash s;
	s.init(key);
	s.update(data[:2]);
	s.update(data[2..^2]);
	s.update(data[^1..]);
	actual = s.final();

	test::@check(actual == expected, "Expected hash mismatch [%x expected // %x actual].", expected, actual);
}
