// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module sha3::kmac_tests @test;

import sha3::tests_common;
import std::hash::sha3::kmac;


// See: https://github.com/usnistgov/ACVP-Server/tree/master/gen-val/json-files/KMAC-128-1.0
//   & https://github.com/usnistgov/ACVP-Server/tree/master/gen-val/json-files/KMAC-256-1.0
//
//  "internalProjection.json" files.
//
// fn void kmac_known_vectors()
// {
// 	// the ACVP test vectors expect bit-oriented output, and since KMAC is sensitive to all three input lengths, we should skip these tests
// }

fn void kmac_empty()
{
	static String[][] expected = {
		{
			x"82e15a606364dd5a887245a06357a3d465f2b05c2605d8d32ae3692d3062bcc6",
			x"fd81c1dd43cf93cf7590dfdc7a65cfc8cb08bce557e109c37279824584af78d97fa76f97583b4d9e27b3e253810f095a2f1152946503c03e5ec68e018dbbcad2",
			x"ea90bdd1a348b58d033d8fc6ad4e9770301376e574cbdedd4744287645c9e5dc24bf9f864ed6767f216c2be6f90b3533b3778517d63289dc5b07a596fe2a6695df5fac26e2f6483e2eae46e7dfed2ea1d1c284a92df049f4558fddb6b2521761901be09549e9f5bee4ad02d07f989f125c3127c680d88115b200335bb4dfcc93",
			x"ceab086d55d44109a057d049f43744bbbd6d6f25fe308010ccf3b1445d1d57e2",
			x"75343305daf03bbfa6b76fe9b01858a154c3e70172518fd75868286c1177366cc30aa2547a5054e6b04d1aaf7331168e14edfc3c51a39502a9c538fb6c82e5e3",
			x"2d8f6aad9c781c4ad960a3f376bdbfea2287d429ced27fbf30b832a1bfca98005db0731d7efa0bb05abf1f5ea58913158968a8c67f0ce17d12342be06ef755150b661938acdc58660493e4facad30298293478d733800884e624b0fa84d9b8079e085a2cfb2c0f12a0cc46561ae8c7c1b4f02c55ed82a36244ff567f567c4fb8",
		},
		{
			x"42901059cf719f419dfd8b2fb5f74b0a7d7777ea47960941a0ad8c97a141aeb8",
			x"6d3e6d0245a8e57f35a1444138f568d5bf87845255c4af913521478d85eff82d2ee1a5944aa13bef0cfee83b81101b79390bc7e22a52bbe5352ec56c9364d2e3",
			x"693dad643136d5eaaf166279da69a2abaf9748fdbed6029b840001621b249548e3603dcc877ec5c6f67314ef22a758be182337c7cadeacb63b6e86bfe2760a151627bdd6729ceabf0994e2b44de8f96f659e6fb016f43267e125436bc50536603582af4c280e153b7237c84ddc3e7062226b10843f25912461d0bcf01e357e02",
			x"9ef410c2aa31d413f660169784c18a7743c43ddf807a8fc8d57d1b1f33e4ff51",
			x"c2c27a77b40fd122f6bbc5c33ea1a0e1645c8fab8d20a60394af0eb510917128bc63ba614262882980c787fc165e00808eebd6f297995c1a932e40f742065fbe",
			x"011b198b98af49632451dcdc32dd22bb9cfa9a3cf157a2798840daa7e7f52e52918270c6a8cf46867c8af951be1c8a57fadc1d4c8884cb8f7aad503211e527787010976c7d93af2682eaa0c24d6e8d1f466af0af404fb618b9aac34a4b4d5dc12a9519cb543e4ddb6c775c9acf7b549b41c293e3b4c52fdaa28a0e9876d1627b",
		},
		{
			x"d17509b0acd9e9e0f6c36e61445614587aea8feb94387d57210841b2371b70e9",
			x"17298a06e713671682eaa903cd192368ebe6a5d8b9ccd4cf01a86f81026e70b59979999d119ac74963c0e6a023dc29131eb3e64b83a0e817b03912a139d5a9c2",
			x"77b7a06c8e1d3f8350eacf1d16874ca8abbbda265c4b57c089826300c1eeeefe456fecf285463471f9cc599d2697eb2f327bd8326c8410fbb8c8cfc27963907d3a9668cfdda25dfed87e2cc4b808c0bd9efa5f3acc09438874abf8b0924962d350b29f6ed3d47b6059708a63822d4dbc220b0d24753406dd9e6dd18cb81ff153",
			x"9c5cb60c1a198243c5f43b8eb301d6af84c2fcbbcea79432c8113a005ec4ab86",
			x"715532f43f6f1b327bb4c9066d9d4d04a1b870684df8c4b2a82c774893705e442c8ead7e5f50c053d7049a5e286f95f56114b2deb8ebac685c10b8b561aa410b",
			x"2ac55a1c1d837c9a75db0a77c4902d49b62fb8d2fafd852956afc412f6be4fa6f609a9d8b07eca54808e81c5ad48337a276b3c34506a25fced52d0192e6a175bf89f794376feca58769dd5a279dbd214dae10a12868e17c075aa14b4d8344ec178bad90caee667086365696098df971cdfae08053497a47631a0e2cb6b510580",
		},
	};
	foreach (j, w : tests_common::CUSTOMIZATION_STRINGS)
	{
		test::@check(kmac::hash(128, 32,  tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][0], "Hash mismatch: customization '%s'.", w);
		test::@check(kmac::hash(128, 64,  tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][1], "Hash mismatch: customization '%s'.", w);
		test::@check(kmac::hash(128, 128, tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][2], "Hash mismatch: customization '%s'.", w);
		test::@check(kmac::hash(256, 32,  tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][3], "Hash mismatch: customization '%s'.", w);
		test::@check(kmac::hash(256, 64,  tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][4], "Hash mismatch: customization '%s'.", w);
		test::@check(kmac::hash(256, 128, tests_common::KMAC_KEYS[0], "", w)[..] == expected[j][5], "Hash mismatch: customization '%s'.", w);
	}
}

fn void kmac_simple()
{
	static String[][][] expected =
	{
		{
			{ // vec0, key0
				x"c9fac6e44508a5765a79d149bed2b8a0c381c5f546a8e653ce60ed42d7081318",
				x"d6dad6aae587b768f05b6c568208b132c536abad78577a59d3f2fd142cf8afdcfc1cd984fb52ec96107a94e1b49d36f0d6bc2ea04f9413c04d7a0b17e13c8582",
				x"8ca5eca10c8575d8474356bc24dfcc6b65062a026ba4ed3307c2e5b9428755f47c8afbaeb9ee60a3e74819cba58de3cd7958d93a7aac89bdea22dc510d6269fa849d1c027a12c163f0416faed9d3b740c2250dafd2486f5b46b496f897c4ddb3cb9cb47d76000b6d9d6cb741e480f5619daa6530bcb56b3e392b0ee65c85e148",
				x"acecf7c701d3845990ca0bddae81adb72335b28ebe4e3c97d2d4d4310393fdd6",
				x"2208d7b835e549d294b9ccbfa5118f1a835bea9c94761eca677484054ae09a3cba55f1a2340f014edbb566f2e5ffe92faf26cc82ef28e427be99d1b4b17a2bfc",
				x"fd1cab95f959ed3e78d6f43e8c8f56f513685899f47aa47c1ac77263eda76ea1f2be6873e794c959ec428bde57b2e347dee5380f42c99da68a835948eb6e5c2fd00e695a168a910edf0daa0b16a3bee4049307cb98a34d91c03cec447dafcb7eda810434779236f2218a0906aa77a16d4933d6d10f323fa9a13a98d8275c1457",

				x"aca08fa702d1b064b303a6ef91af37234be4707c1c2163e93812572ba1da1459",
				x"7f342e19a85d7e868b14fb329b37e59a5775f73c0362233bdb262432e1e736b5be59cbf6322796c08e5afbceb7372338bbce5eefc7260d537f0d48b61e47be64",
				x"c28b6864dc88fdd37f049eda97645c96afac0b19844049c7b25b055efb68454d5cbc3de836392b253f6595107e52771243601e500c7c5423d9a78fe8e9c100a991875ba66de7bbf8743a57db8f24e3d28834e72dfaa28017458adaa58d2e43ad2ff2d2741db8625390fa9799a811e8aa156b4d9c5d1016e8ba64aecd1a9c6845",
				x"335beeb9e7201c3a0269462b004414d9e703476f03dc7e7bb5e361c2e506c716",
				x"6abe8461b4370fe60569ab579e9813851be3d50f1ba0b232ac42ca682e37657ca39246793803ccbfee943b26a3dba317bf226fd51cd9f04f1367cedd74612c48",
				x"e5379faa44c2329786ce9e786753e6065acf93bdda9b106bdf12fe6a8653aa31aeabecb62e447b4175c3742437f917d561609a79645c4b771ca107a6560d29b474933b0f9dd0db3eaf40d8560700961c2764bee7056b197f3f7715cd755a5264ee5b92e8f5deb10af2b7ca676fd444a7b8909c56e4b69a14fabca8547638915a",

				x"af4357e3ba3dd80341d2ded80e1d5383a815d1b8b7a611498d532cc40ddc9663",
				x"b7200c9ffa4427ce4a0879b6f92e7ea7f144361184ec44072a336bafb8d9ea56b674a4808a93c56933cb5ac1eb0b9dac16642a50549051133b12593ecd69720d",
				x"37337301bb29a41166efb0b0ebe38ae77fe5aa64ffaa621fe9e1b4f36f2f5c9007a04eafef0975a1462949f8306152f931260fa77004645884f6a5398f6bd9cc55623f4ea34744775cd942d2204c24d370ab95a9b72430216dc0401ecfdbeae895b1137c1f8bdd899f34752404d6f15b112796276b02da25b50b6979f11d5746",
				x"f1ce174519fe62d8007ef3a078d08f81c1bb3be82a48c23678c78a2b30c1c9de",
				x"7a15d356fbf6d7f6b738e4499c59463a7d31410f4db97e3ab2c3807fe977cb76de6bbb7b814e300371c57af6446e58ff992d4c98c43890499378820069d8343a",
				x"7bbdfae2cb4c0d588e2925e60d6e5e4548bb6fa0813fba1e377dbfcda5c248514d5999724e88771d2d33a4826e7955dfc87bcbb2e0047f74a4564ce7ebcba4497b4abb2d912be6fa5fcaa626f9b795f34fd27f384621958582cd6cee5b924892947f48b4f9726132f75b86fa4f7536e94241841a75e37d43897b3db3728ea192",
			},
			{   // vec0, key1
				x"8c189fac0ccef0887a891a45ecae65ce374d7d993f5bf356040724a69cdfd938",
				x"47420d108bc341ef7ec84b31b6bb1cca3770ba5c0dfae09b88f19600fcb3d244a24c7a9990481e3b59f9523e703bbe856d6f65477ddb88aaaf02ccf772b4c78f",
				x"7186373a6fd7c228db2d648d5d9436e7393a2b5548e781567b550996ca7a10f0422e8a50fe454567cfa829dd1193d9e5f3e20c85997624d482516d488bef5f07160bc130322a9071705afb47580f6f651aeae0d6252368614541c339333b5f7e17224432645241ab5cb5ac57c33f78aac4ee6dcd37e2c84781ca7969b3cf22f9",
				x"efc50436cbd47a73c7a3a8f0d41df984c6b9b6508e62ebb4052edfa5417334e4",
				x"528385d5341b09f66bf72e19cf2813e0a5fb16a8874170056ce92691a674ab38f72ecf622b528e4bc79070ee9e1a2ee26165f9200fc218f6acd6b3a8cb6efeaa",
				x"aa377389b08789b9a44849bb3c247eae897cda0ebe973266e145d82db1ad7a7e82b20da679e29830f7e699eb0cb7b1c42ce7dca84a31c1d59c0cfee6c4b686097aec6f65aa2051f2cdc351b2df72d5dd4d49abf725a14346bac1bc773338505253528a869328c17190fb3d351cfc601d239d90b86ebd0e13e47eda5ce53cec8a",

				x"52fef405bb62d99180c96f21ab1530ec6158e21d1ee0b39b1f8e92c7740fff92",
				x"ee6adb15e2c68c72bb096733820d533e9aeb2c41dd4c1d47cbe2b82309c1739aa585b59b986c2d34c9657149c872bbab41cf9e88f0febdedf12d50db219c2eb7",
				x"efbe01592b97e5f305189a0d0334085f39b4fd7611de568968b9bfa8ad375f4ae4af0eb27bdfb1100f3e816601c2da325c9a65ebf99d7dfe978ee8c8568c4c73e2b74b8f12c8465bcd7f6deee29f81e0f9c34e84d388c247a84f3e994e8b0a4310b462d58bf57eb1ac7410e9a2493059580b55267b0f226f41e73ad1bcf8219f",
				x"55be0a6cc90780aaf8975ca72914b22ed19f3460b160d7bf9481477388b03fde",
				x"2ac4f730b61802753ca2de05a49ad7dff03f3404d0fc16097cd164198acadbc2b5a1dbcfe17d2011022c5afd44cae8ecc42ccd8a608316a3284e69f2a045b119",
				x"6ff765a1354e9900f37338ea32bcd9bc8bd0c1ad8c726cdeb5fd6b25eacfbea64af244c7ad70ff9ff4d0438e83f6e8cf40baf9cecb9067e9e832e4d5329e578c2d7e67cd33916b2676fd99df7bb4fd52bf084be08789d785a9aae9f28cbcf0ac3d9c6e47860e1c629ef47fc208d9dad31c7475190380edb2c37ea6b6a541b7ef",

				x"b2521a04b8736dc0ca3b3d7838a1f3dd47d3ff0a1cca23bba2c078d5297ca4dd",
				x"cd9c942b65a840c6696321dac6fc7cdaaecbd0d54cf13a41c24cff067e4ff7e88e3904e3b30f6a618226db0fd33ded7de38df13f1ca3d82c23ecd2ed5a6b4548",
				x"a9cb65398a37f17022164dedc78be0aca527a8279434eeef84e41d68e2106cb5e7b784a7415b3dac4610ae8987291056aa026e0eca16e547d3c118e35fc4a4682d85e44828b7f552d302337cf8c851c19f2e7d6db87a250c65bd332e725f5d7584882b83c8997643f806105f52f352045a6c6c154b0673426ea6389a9ca42a59",
				x"0dd837dcfeb2c684a81dcf263fc6041e27697a18bfa17c726db2cc82ea239988",
				x"7cb2c533e1a8b479d4ddde2e5d7c47947e8782f28902228b834758d17e72f09bbe43aea6cd27c756ce1ff3c8e48bf78585c213ffe471f6b1692a97e2d551a7ed",
				x"132ff68c28f9f7464c585fe4ccdf72417a99630b833b5332d643b8d55cf4e6913a70691e8bf86a9da0fff1ecedf0aa9b9279bed937eac4f096100b7a531b716b1063a093391290857a60872dc54c804c45daa079b3806b95979910c544e3a5960dc956a5118c0a744536ed9c1b406f69e473c4a4fca360ff9be9ae8655a8af56",
			},
		},
		{
			{   // vec1, key0
				x"3050c5bd48f3a2e73a166f60870f500694a373c8fc9ef11d3b136c1ecd14ae24",
				x"2a78d97840807d4633aebb4f673d002f7fd781601ed7270120b5bb57ba25de23ee99f8b57cfa1176663f00d1654680084c0c1b94481c8de8311da776ea8533c0",
				x"87cb85e31387826e1f13a72914c3d7b15beefa595a5b01a33dac898391182c7e3ed96f00abe98e4353b6d132d7dec7baa9ff410572561db542524adfc0369689c54b60015d0565528b47a666f6eb40014ec21020661f9568e76014ac8aeb8a866c828431ca6ad8f5ff459791e13bfdb070a8d22502404d9c71bd54e7aa4ddb08",
				x"2dc7ea5c385f1c81beb2e4330e7d071caaccae3c6719064e44cd74bfcc673ab6",
				x"b88a885e1dcb377e8c5e6fb33fe046f15b65ea0b560ede3c660f8a8906f007eced7feba1219b353b65a2e3fa38795407ba7a1c06cd53d89f80a87e2a1363a744",
				x"d5bcc74f25fd7ff7506c2eada212045c58162d5832afbf9a2699d53705ae933f0ef8850a21f00035f20c74d417c67a6034c6fec7e9d691e6bde3d215d9f8e40558d25035b94b0f912103fefb73a6a5785a1a2f9978762fc3950d692a34ac77dc5b9359d6669ba2463237a0594eceee1b60f4354810f91abd8836963017df5be4",

				x"af9f63a9f16ef253b89c4c125af446ae15b93e11a0236d39625f4ea5a757dfc4",
				x"f90c097ddca28240724d44d1e7f81333e0ae7fe494c5192e8cb781c81cc796d5a393d710b0a2999285f129dc12f8857579773e8a8c0beff0e8069d29162b76dc",
				x"0f477d91b3530e334a18934110dd9b33e88a576495a9dc39e89d9774b59b013ee41af6d61b3c636f9d35ed3dddc616784ed5965040ce46da93370d5a10bb6fa22935b738f38220248f212a22a90652ce3124daad7af9e81047f02d1eed6fc9a69537ab48a60c08f02216505b9c76356818a429a9ae547dd5627ad83f3e497c1b",
				x"17a204cb1634f9c5922de8072cc4c40fbf0ad98c65d83ae61c001028b6c05e5f",
				x"2c6edda3d2b8fed465148d7efd0bc2a0e4d7347a906ddfb498fdb847c1f03757171a22e77d94b415b0473e42dbcb753ed48f55490455224d7911b39c517edcd8",
				x"78ce050ebff02cd93c5f33b76963cdaba9342b14f73bfd8d5a25ab32b30136ab2412ae152e859b12c26a1c9252bfeb57032038a088645ec5332314bba8ba8cc05654716c61f545c0be456c7face29c40aad32af88590a8a3c7b5f9594339c4c557b606046459c2a36b75478bb4c45eb06538de7f254cfa484f7414fc57c76426",

				x"3307824476ea878d7f88d6fa71d84d5fb08d2772d8532b95b14b310b11ec2de3",
				x"cd36126e9cbe9fe2e7a865d3fcf95e6306700b74d51b1919ffb82e27192a67957273d2a22b362528f021a0aeb490b994cc224af288a8304d666eaf25d8aa451e",
				x"152a18a003a161dd8e5f4f68201e2f965de5a68a78a0b35c4d13241a9dd556ddc496c24f3594a733531b91af04b8edb1ef3323bf9d165eba92d551a78f593ae664d5ab617bb6f54e55ec2f6b48e490bea44643188e717c89a2a5695a478408199e3b03b6a1ae1594f12aa82baea58e521fe7cf23cb793dc5dcc4872677c35925",
				x"7d3047ac3d2f37fe753d71a87fb4ed1962af33b927d53f0bb45f2c18682cc42b",
				x"69fd6e83fe44da93327b6d891d62ec3133da0ab34feb13414c1e237855651e32ad86a48f3e77a9fdbac92e57d2d253c24515a5be72f25ad421e6350e8cd78722",
				x"933c59141bbc64d6fdf93c9185f3be84a8f9557d60cb6df47c31378f117cc21b82c7e6a5b0d0bdab7e2943a0a9065a789cfe28ade477327ed1950407d31378f364c518bfed3cab2378570a396a7c0be985f2c97cb7eb9db304ea025452ddad2d8cd83f49351405051c940a92390880b1d2063fbf21b7a2854138b6e7a2ec2d26",
			},
			{   // vec1, key1
				x"5f185d6c1dd4c3d09988b460c78e915adfb0a94c18ed65d1648adb2a09205883",
				x"ba2bc9ec5998668035c3f54be515fd18c96536d6ae52117385319aa99cc50ec79397ba21c4c088b60fda5116ac1593c5a64a62c5878726c0851c92bf412a6acc",
				x"54f667056db66a1f385d5d3a03a7e6fd33daee02305c5663f675c496a5c145ba4a6d430acce2bc011d1561a5175e0f778669b4d1cbf0d4ea57861c02f33113b18550c67ebcf4f57cbc4552e10d07387320e83179d54c2fbd83d138cf77b7ed01a72f33f64401c88ac10414776168b81ddc3facb8aac73aa06b7a07770459358e",
				x"0c1d10de9da78067dfa1284158cbe710a42a8937589b9a305f479015384f2b33",
				x"de8a5fcfe9a4110375521cb4b59d8a0372a07d27a11007712140bbf273812acc2caf13262bef164b63ac7211de4fd8ef218cb03b4ffa82d9807d48dcdcb011a5",
				x"36530ae1bedbccc2dc7b3e856a67c03e1eceff1da49984d66d71810c50c65310a81cfc4b789be823a3e00e24bbe6a4d4a88c3f6558cc65e69b24b7f035d87cf76b7e654449a0cf5830fbba6b6d48236bdce06293119f92a9d8ee32518935c51564f4336327c2ccf64f3a7680fb21f722fa9021d716bd6ac8372abcbc7d1c0e23",

				x"c508c2e931cff9db631028248f4cb6cdf91b34a4871727c29be6e964686faebb",
				x"bd06c16c277f945184752a580fb60a2b0d240a1ff42b18b4b8c155c664fc45e279693dc0fc0872ad82a9cc143645fffe05b9d26d9c5c6eec40e4971e28bccec2",
				x"ceca39dcae39f2a903288513ca5c69b721e992f2bd0a705b05aa9c7de270a6b70732ca712e9039562d31661237f7c92817e49932a640a4abdcde2b803b92d443af063c59640ab3327b180c5698aeafeb2a432cdfaff44b655d8d096485c17804c5864adc390c4fa4bc2915897e74367b0833d885d848edc2504a5341377d1b7c",
				x"e23cb591be3193f18de5169f3bc06035289c0f4188f30a0ac44ece8fc40605c6",
				x"4f1d32a2c35a169541e51f00d437259110367e5c9f88525e799995a29b45f7835770f343c9cce31cdfbd51cf8b808e53c6ee3d828c35f8f1da1b107ce2723369",
				x"17b7a33ea58c92d0505dce02538df7d37ac91e4f842b1b9d3453857551efa9a9c38f6ddf0b5661d035a583b92daadd0d8532c3671c0cbd1749ed73ed459e36017d73709ce1e8ad65672d74197009555fb9ed1a4ff108d20022c8d800421835ea3366a9abb93f5275af4b436f8df13f25ceee78ed8c1f4efcc547603cb0a168db",

				x"addba0f22c39d477b86b8e0aeccddbc320a69070cca2d5b96cc2e5497dfcd65f",
				x"ee3d8fd522599f2d1644b53affefc5e33b4c6bd1360fc9af17149f28f5b28b96da78f094fd2be63572c67cda384358e5af5a2409ab1be3586a167c61e93e8277",
				x"2c8a1db4fb774e6eafdc876493fe60fb02d7b97a24572b19aff8571d35560cdf4c0786fd485f2d9f8b64f0a7164c12ddc79191341017afc59fc16e9ba8e2f7ffb857730f934071e88ef5c0554286064a6c16b1d5cabec6621ae1ca0eec3e65e91631036dc57ed2e0cb4b80220e5710d20e6127edd2d60aaf7f647ce87c8dc32c",
				x"1c11e1f5a14506f0d7f79b0b61f4889ebd0c21a7adda441b79352319ec49b06b",
				x"29f5ac8eea0ebf3c6aadf565ff41c0cbef5681dcb0fae298d194c432d43fe59bb8aa4d1416f6e83a6c2c4ee0b5ce30fa370af614a57ef795b3e6f59ee0bd1e0c",
				x"9d58b19067ffb4ee9f38e685caeb2c05d5acc28d9626e44650d8a664aaea082726870f7da7ed602715fe46d05c29bf6a2596881da380160bf72393d78306b1ecaf2fe147ef26ab0ab7c5cc8b1acf5b5d0db5c53be962738cfa505c9ee4ca74b5a5c4d32747e14a597b5a29baca3e1e112a127cfc4d14d86135e302e2f2a46081",
			},
		}
	};
	foreach (i, v : tests_common::SIMPLE_VECTORS[:2])   // only check the first 2 vectors
	{
		foreach (k, u : tests_common::KMAC_KEYS)
		{
			foreach (j, w : tests_common::CUSTOMIZATION_STRINGS)
			{
				test::@check(kmac::hash(128, 32,  u, v, w)[..] == expected[i][k][(j*6) + 0], `128(32): Failed on vector "%s" (c: "%s").`, v, w);
				test::@check(kmac::hash(128, 64,  u, v, w)[..] == expected[i][k][(j*6) + 1], `128(64): Failed on vector "%s" (c: "%s").`, v, w);
				test::@check(kmac::hash(128, 128, u, v, w)[..] == expected[i][k][(j*6) + 2], `128(128): Failed on vector "%s" (c: "%s").`, v, w);

				test::@check(kmac::hash(256, 32,  u, v, w)[..] == expected[i][k][(j*6) + 3], `128(32): Failed on vector "%s" (c: "%s").`, v, w);
				test::@check(kmac::hash(256, 64,  u, v, w)[..] == expected[i][k][(j*6) + 4], `128(64): Failed on vector "%s" (c: "%s").`, v, w);
				test::@check(kmac::hash(256, 128, u, v, w)[..] == expected[i][k][(j*6) + 5], `128(128): Failed on vector "%s" (c: "%s").`, v, w);
			}
		}
	}
}

// Interval tests are skipped for the KMAC construction.
// fn void kmac_interval()
// {
// }

fn void kmac_streamed()
{
	String s = tests_common::STREAM_VECTOR;

	Kmac{128} s1; s1.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);
	Kmac{128} s2; s2.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);
	Kmac{128} s3; s3.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);
	Kmac{256} s4; s4.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);
	Kmac{256} s5; s5.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);
	Kmac{256} s6; s6.init(tests_common::KMAC_KEYS[0], tests_common::CUSTOMIZATION_STRINGS[2]);

	s1.update(s[:20]); s1.update(s[20:10]); s1.update(s[30:1]); s1.update(s[31..]);
	s2.update(s[:20]); s2.update(s[20:10]); s2.update(s[30:1]); s2.update(s[31..]);
	s3.update(s[:20]); s3.update(s[20:10]); s3.update(s[30:1]); s3.update(s[31..]);
	s4.update(s[:20]); s4.update(s[20:10]); s4.update(s[30:1]); s4.update(s[31..]);
	s5.update(s[:20]); s5.update(s[20:10]); s5.update(s[30:1]); s5.update(s[31..]);
	s6.update(s[:20]); s6.update(s[20:10]); s6.update(s[30:1]); s6.update(s[31..]);

	char[32]  s1_hash; s1.final(s1_hash[..]);
	char[64]  s2_hash; s2.final(s2_hash[..]);
	char[128] s3_hash; s3.final(s3_hash[..]);
	char[32]  s4_hash; s4.final(s4_hash[..]);
	char[64]  s5_hash; s5.final(s5_hash[..]);
	char[128] s6_hash; s6.final(s6_hash[..]);

	test::eq(s1_hash, x"dad98b3c49eda89bf99ba59bcfbcfc691128771fb5e0e57a1b7fb5bc61e870ad");
	test::eq(s2_hash, x"b1865d92b6dd6a32003f2a09664843c22f81484fa46e1ae204da8d704d1aec94b86ff4707d188b3abc302eda85da61aefbd04b2a6446488458d351ad1ac6e4a5");
	test::eq(s3_hash, x"4d5f6922f76cd4b8b354728c6d52a82d9b4ad7696e8959dcf27b35fb599da98c163d7b5dc1d8d766187da92cb8e766454285dba2dc4d32bf8fe5db0ae0e3d90fc118a15e8706f6e6e8c4c054649e6820756c949aadc637cde8079410cb72589883e6974721a9a19311e47763963982c7a2313563464ceeb40facd9b047f17737");
	test::eq(s4_hash, x"10ec9255a7d649236e30ad812993f5c17fbbd684a1738d6f00966cb34e1749bc");
	test::eq(s5_hash, x"b8440c5ebe4d3517a91c2f123f116878ea657c29593f20870270f679d412ffe8b0e3b6e1261871004f93f1d0415bb9007376f69a208fed222b67c60573acbe9a");
	test::eq(s6_hash, x"0e465ce8ab9a0562e9fab2a0c1f1b429b2d88565d544bc5b7440439104c2f92db8703ca6ad041cd576a718bdc85dd45b16913e9e815bfc741a156c94c9ffd7b61aaed772de53b877000124bf9c667e4d06d6ebf911a55aaaa2f1d84af71280c86bce35a062f25d1c64458b7a94e4a51073651484da4b175c75eb9a23a5b8d92f");
}

fn void kmac_large() @if ($feature(SLOW_TESTS))
{
	static String[][] expected = {
		{
			x"b615310f5acbcbe28079607458509a248c5ffcf94e194bd502dc31611b490785",
			x"555c74506cd32f3267d623f3d83adb1a5b107124836b7df61c32ed1f55eb497edd1b292f763a4fa4e3f6147f5fb944a52fa9e4de2064613c657810a654efa363",
			x"38b1a36fbbcb3a634305eb1dafe104ed66746b06b6a498e5bb2b897d0a9da550b494496407bb2570d34f88d453b10f44b4e91498af815a0a18952418a578b4c8431e83a3e4826bd17d930465b8e04a6985887383596fe3bc15e286d236549cf2a852640553e396b53099a608096d5e01867b0ac480b3d3f0f9dd2910668ca746",
			x"34e70bc29cc2f2088da42aa842b9a655b68807ea4f5454930c80c4f8578693c3",
			x"41a63b82bc87d9f26249d15d0159a1c413833cd36ff3de9d329be607326e52e8dc194816d0c74e0f9f684d6f7fed9bfad4ce8cf96fea1ae14737eb73e650a193",
			x"c43ca982b0fd3f360d2ee9e04dbacef6207995f0f9d2b94e5a7f623b84c7c4dd16b8dc8e6c385093088b5a7636ec0be41907c933749eda23766264133786011ea446bc4699c06ad1fa416ced59d9959e1f03b1e6d9b2dfeccff68b329ce19e82f28ecf4628d7ba2e5ab5ea24bf82289664fdb85826a695ba3d6216eba77c933d",
		},
		{
			x"188ba676915e51f5b319992c71842c3e9a08a9ab6e590aa7a25b65e8f021b33e",
			x"89c30693f3bbc59a89e775ad96a59130a67f594a4e71586494f27bc91412d94dd5e8513c905d0cc2e6900f19604c15c37f7983a767bbf62efd2987f74b5d6cf6",
			x"1c84b7da19469e048de8f692f78ea8815024593953c2136b07ef1f2c5c531d90537c14a1dfacb2da7e2a17ca54d8b0018dad055464df4d71c39487edab75b1dca4c2788096abf602c180c417c98b22d0c4457281e5b37ebf09c3ec643b6001b209d07b5c71d7f4ed2ff279e83a6f1e457da3521d174f3965ac450f8e83448d53",
			x"f45eca6fbaa67c69525f4fa3473980c0605282b1b0085d20823fb6a4b5a3dd6b",
			x"0641d54784e1ff39e8fd81d0604725569af63016a0ea36be3622b78160e04793f4132c5545a75872d6644b68075f2afa2dd995cdc98bdd91fd1517cdd097f320",
			x"fae4a26b4ca748023fa1b6120dd2b0a0f09692b2ed7a6e39eed9ef11e7013af4de34e28890ac33e38bb10abe5db2b956b656bfa16a33f5ebe72c7436d129175112ff285358b5375dbe500fcf87db3723fd797162574302088797b12652e71e6041d655b2d96d49aa4fe27fe8137e75f9164aebc57c03570c8817aaaaa8c15863",
		},
		{
			x"5f03a97ac26f91ebd77cfa4732daf5cd16ea5bf995d9ad32d2cf200498d661fc",
			x"b37da22e17fed63e954fb2111cb5eb127617e9f3e05ba8e037c932f840ab9b1e2abfd239f3c0882654e2d4cf5e5af7b2cd4e08736d49508422b172cd7537a306",
			x"64bfecccffc4f8d0b5f0b0cbe0f6537b509742694b14f8446e2cf6d5328c2336b077dd71d060afd23ce5bc46ae983c540b056734b87514db5c784c4dfdc4a1ac15fd9c82b2edc83b3f4fadbb367828a9d8c9916ee88a6fdae304038ff0a5097d7c29f45fbb0e3dd07448044a7be66fe143039317e82cfba105daaf6a7679efa3",
			x"efb841a5c227b21e92f1400d651d109b7f747b4460b819a9cf46dfaabb5f2abb",
			x"b4fd04b48f4b6b48757329761ad38d739c49bbddc756724676c15e43dd710e4f424520acd0cbf5598bddcdbb806c41364cd26e9715f4bd78fa369512d3818c2b",
			x"6ae4d5e3fa66e95385f2581afc5731c11ffc43dcf0d4c7bdcc6e0bc84524f1c4e3841154e8957a45c2f5aa864a25b3fb72a80f911131690cf6a9f0154493fe494ea0ade1f5f2f378fa57017a64cddfa91d7cce56a2ed790acce1d439d93d315de2632cd41affab3e4de69ee7bffda603b4d5c3b7ca3445659fe14647b839e850",
		},
	};
	char[] bigboi = tests_common::get_bigboi();
	foreach (i, v : tests_common::CUSTOMIZATION_STRINGS)
	{
		test::@check(kmac::hash(128, 32,  tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][0], "Large hash mismatch, customization '%s'", v);
		test::@check(kmac::hash(128, 64,  tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][1], "Large hash mismatch, customization '%s'", v);
		test::@check(kmac::hash(128, 128, tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][2], "Large hash mismatch, customization '%s'", v);
		test::@check(kmac::hash(256, 32,  tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][3], "Large hash mismatch, customization '%s'", v);
		test::@check(kmac::hash(256, 64,  tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][4], "Large hash mismatch, customization '%s'", v);
		test::@check(kmac::hash(256, 128, tests_common::KMAC_KEYS[1], bigboi, v)[..] == expected[i][5], "Large hash mismatch, customization '%s'", v);
	}
}

fn void kmac_successive_xof_operations_128()
{
	char[] expected = x"38742f2e6a0edf501ba625afed22fb6879d2ec0f10ee9012ac410b9f8bce51a5";

	test::eq(kmac::xof(128, 32, tests_common::KMAC_KEYS[1], "i want to be sure this message wasn't tampered with", "!!!!!@@@@@#####$$$$$")[..], expected[..]);

	// Now use the cryptographic XOF functionality to squeeze variable-length data out of SHAKE in .
	//   We "squeeze" 8 bytes at a time, and the resulting value of the XOF, when concatenated, will match the expected value.
	char[32] buf;
	KmacXOF{128} k;
	k.init(tests_common::KMAC_KEYS[1], "!!!!!@@@@@#####$$$$$");
	k.update("i want to be sure this message wasn't tampered with");
	k.squeeze(buf[0:8]);
	k.squeeze(buf[8:8]);
	k.squeeze(buf[16:8]);
	k.squeeze(buf[24:8]);

	test::eq(buf[..], expected[..]);
}

fn void kmac_successive_xof_operations_256()
{
	char[] expected = x"f0b8525e66f4d3845848f198a57bd615057f66becd1ede603f6110a3f5e91e504d288267907c58f5aeb63d301455c515ab184a";

	test::eq(kmac::xof(256, 51, tests_common::KMAC_KEYS[1], "i want to be sure this message wasn't tampered with", "!!!!!@@@@@#####$$$$$")[..], expected[..]);

	// This one shows the even though we squeeze at odd intervals, or beyond the length of the original expected value,
	//   the prefix will still match.
	char[128] buf;
	KmacXOF{256} k;
	k.init(tests_common::KMAC_KEYS[1], "!!!!!@@@@@#####$$$$$");
	k.update("i want to be sure this message wasn't tampered with");
	k.squeeze(buf[0:7]);
	k.squeeze(buf[7:13]);
	k.squeeze(buf[20:20]);
	k.squeeze(buf[40:1]);
	k.squeeze(buf[41:3]);
	k.squeeze(buf[44:9]);
	k.squeeze(buf[53:10]);
	k.squeeze(buf[63..]);

	test::eq(buf[:expected.len], expected[..]);
}

fn void xof_vs_non_xof()
{
	// iykyk ;)
	char[50] buf;

	KmacXOF{128} k;
	k.init("A KMAC secret key", "");
	k.update("hello123");

	k.squeeze(buf[..]);
	test::eq(buf, x"628c2fb870d294b3673ac82d9f0d651aae6a5bb8084ea8cd8343cb888d075b9053173200a71f301141069c3c0322527981f7");

	k.squeeze(buf[..]);
	test::eq(buf, x"7b638e178cfdac5727a4ea7694efaa967a65a1d0034501855acff506b4158d187d5a18d668e67b43f2abf61144b20ed4c09f");

	// but this one is different
	test::eq(kmac::hash(128, 32, "A KMAC secret key", "hello123"), x"1fa1c0d761129a83f9a4299ca137674de8373a3cc437799ae4c129e651627f8e");
}
