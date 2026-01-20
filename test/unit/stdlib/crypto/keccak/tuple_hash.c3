// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module keccak_test::tuple_hash @test;

import std::crypto @public;


fn void assert_finalization()
{
	const TupleHash{128} EXPECTED = { .c = { .under = { .k = { .state = (KeccakStateMachine)KeccakState.FINALIZED } } } };
	char[] expected_view = @as_char_view(EXPECTED);
	static char[32] result;
	TupleHash{128} t;
	char[] t_as_bytes = @as_char_view(t);
	t.init();
	t.update("C3");
	test::@check(t_as_bytes != expected_view, "Hash context was cleared, but it should NOT have been.");
	t.final(result[..]);
	test::@check(t_as_bytes == expected_view, "Hash context was not cleared, but it should have been.");
}

fn void outlen_change_hash_not_xof()
{
	// this doesn't appear in the test-suite, but notice that varying out-length values for the _hash_ subset of tuple_hash functions
	//   will NOT produce substrings of an extensible output like the XOF variants do
	test::ne(
		tuple_hash::hash(128, 496 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32],
		tuple_hash::hash(128, 472 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32]
	);

	// this doesn't appear in the test suite, but proves that the _xof_ subset is just using an underlying
	//   EXTENDED output primitive (keccak output is not affected by out-length)
	test::eq(
		tuple_hash::xof(128, 496 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32],
		tuple_hash::xof(128, 472 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32]
	);
}

fn void xof_neq_hash()
{
	test::ne(
		tuple_hash::hash(128, 496 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32],
		tuple_hash::xof (128, 496 / 8, x"abcd", x"efef", customizer: "bbb123123___A")[:32]
	);
}

// See: https://github.com/usnistgov/ACVP-Server/blob/b121fee031dc65d32c96f4109a42a10dacbe2076/gen-val/json-files/TupleHash-128-1.0/internalProjection.json
//   & https://github.com/usnistgov/ACVP-Server/blob/b121fee031dc65d32c96f4109a42a10dacbe2076/gen-val/json-files/TupleHash-256-1.0/internalProjection.json
//
fn void known_vectors_xof()
{
	/* tgId 1  / xof: true */
	/* --> tcId   1 */
	test::eq(x'2a1734392ea30b0e48fc1358dad70daa234ff87477525b80e1be33ec32074eeda4e23870881b',
		tuple_hash::xof(128, 304 / 8,
			x'dc246a7b5fe7dd2e63df4c1310fc30839c65995d2369141dff7af80481630b7b6f4b634c587ffd12f06503a5ee71c7ad150c08ba4bbaa24070c3fa5df9ce9366843b84d7a94590a51fe518c6aa85e0fad7f2d315ca90b7f4230d91e9241cf7082bad2acf6b9ea0e821d24a02aec54d02ac1f79f1d7bf14af0bf076492db27869',
			"",
			"",
			customizer: "V{6?N|`G4bJ@:QG?.p^kO81h<]*xNxFk2I7QVhBzlkhf77tz01dG)iI?kM$qsfx;<^.o;uhEMc}cNJP%g.i=4}bH ve)eP('rMJW)&`9H?yEjeo@{}+e|Z'={eoC0`_|[wp! x{bMOtt%<]:(D-fEA*7"
		)
	);
	/* --> tcId   2 */
	test::eq(x'998a3df351f19fa14c1c8dc6ed0b7c5682cedfa801023ac056fcf01136f81e91c5b8cedbeb26555bbefb390102cf84184631641ede332cfc86e8bedf3d0684',
		tuple_hash::xof(128, 504 / 8,
			x'b5659db1d8521c31e732588151f86a196cd428b8d68b6b3b7254a4db77f2ae543235399aabfad39251b24c13d223fe7a711e5a919fb67fee',
			x'5c1e0539e7c7e1799cfb11a68b8e612560d0919da1683bf0e4f23ae03c1f5c1e890c8c654feca2f9ef2e84d7605d664fd26675350b23dae883552689c1d0a721d37204f1dd348cd5059dc17b51be450a7cfcb1e34030918dfe6999dcc1a897b0aad696a668e24b844a',
			x'14f22a55d8ec9acd',
			x'82610de281fe032a8cd267518c1c9c707874',
			x'796ffb0af8a402d40c1deaab0b72fec3',
			x'126ed602016bb46b84942f82dac24eb80f7e85cd68a1f367d377dfb74b5d1adbad146090ec805fa12e16',
			x'57ee3f5423ae232c6c0fdfe09f3e050f65a5903c49f603650b5149a8d57d64d2bc752da191215053bdac199b4b59fd032f617b76cf718e8590d1757302e0d985ab2b3a9d413470e01b60aed87b933680c54b88ae030ffdf6',
			customizer: "-9Q,eqH ^j|G!Tfr:%aO0,[L#FkGEzEzT9] L/G2!)e0sU?1Sq}-p0k [%)>&!YcD!rgvfF+]NN'ELXK'ZrS{9+DM]CH'), M#jqcV:mKrOBadZ0K[7J.'f,vM::B}%TBMWCafgNA&))ihch!4|IparG"
		)
	);
	/* --> tcId   3 */
	test::eq(x'aa14f263dfcfb96e3d169747d7ec9d8115e50f95bb39ce9de6b00c6a1c7b54d54ae3389788b741f727b545586043cf823e127e1557',
		tuple_hash::xof(128, 424 / 8,
			x'93e286b2c6ae6b65e6982704ca7b30cf7da52ef52da180',
			x'df25dc54750c50ff1d98257648',
			x'247b20bf9443d3f2e8c624565550e3c35f97f661d7234c8900d25063b6be73e935e936c014d1036f240e8af5267b32c40b7d3d4469da275b3c359198b2f70173bc4bd18814ee409ddc603cfe7e89be926a035ab7322f3406724baf546fdfd0600d097fe50e9dc765356f749ee199a6ec89007883570e7c3b30',
			x'51a1d27623f937a1baf6f0bf3298f414427f09538e3d039f08a15492a502059d68d7353923661703a924bbc6a93f9ed04d29ca10ba811e87dbf5dd3bfce0c676bc99fed068413d0d90c3d5acc4de94e5dd50726b78c23766a89d7a6073a7c77ed180a53d',
			x'37fc7bba83c70b1c1946',
			x'927d5b5a04480fada9a59c450e2498f955967ec20a',
			x'0a3c33e687351c39907b74ec3a48eb61cd',
			x'c7216f800c7da8240d1a4e92613ca57f20607f7ffb6f70a1b55cc3528bffe2',
			customizer: "Zd@N'Vyg~_ rs7rEyJFP>(;LHdl=Q{}3q?<^&`0O:8OSD~c4{LOcJ$mSHr'Bi[5$`m Sz5@9:v6&Vf)H XA&L;gmPg53Ymf^#LrU~/9f-6HSmg{bi]A0:0VD&3$3 ._L0V$+rQIq@Yw6r3 ijRH{KUCq"
		)
	);
	/* --> tcId   4 */
	test::eq(x'85a3ad7601a1b63ee6db67cb41111128b7a93b36506a90d3458795f05670e1b0a98383f3d914013beee2163c7f830cf8699b5980b2f8dd59a1',
		tuple_hash::xof(128, 456 / 8,
			x'5017acd0',
			x'8afcdb53e377865654c07dcf1f6a6568e5a79926cb3f78c81c5f1c63a20649284810e47468133ee94d3cbc4671939f86b7df14ec3a0dd46f7143672161d4931007ee63bff11c48fa93d334a0aee90c83817d383617',
			customizer: "0R&8nqO`~rm{|0vE0hGev/E~RgFj?AQY$DXE/$*A8a~OJV$B @Bkun$ .&d'KeA]A3wWUQ6,_&M5/tT{c%j}}ed5AX8*>#uR@A.8^KlM*7-oAmq8PhBFzB_[Mc6|9-l[0ztlf=>87<tgXsrSf~XU_NW}"
		)
	);

	// Now for 256-bit security
	/* tgId 1  / xof: true */
	/* --> tcId   1 */
	test::eq(x'e3b2ab0dd3b26db3ec646d61a8466823a5765d6e4c024a5aab03592e9827944265355c441dafc056',
		tuple_hash::xof(256, 320 / 8,
			x'6189aa2d5d862f4f980f47cea776cd40f2f8db1bc0cc3a1b7fb3637ed79f51eb0ad6038d1a1e97626314378b6614e05a46e0e4ffca45e96517c54bf5e89263d09aade388d11320d7f7a660a93152e0cba84c113b70ec99ed5ed4a13bf68e08c3be69dd47e089a7571b55eb6332473ab8004283f156',
			x'2a1f21f5a2668db86fdfc2e2c994e79f5097dc668dda82a803b40cbaa17d548967a5019fda43d71978803d6b3afee4d5c9d101be00e7fb2cbffc387a800638ad4afce0ad6ab0705255dd4e880742a43c2e8cc0d92b9f8647536b504a952df6c70845addf3a12',
			x'0cf62fe799ccd76ecc2d65e637af63860e11a52ce80e75c785dc6cb83bf3403c11f363e892fa30c80f308369778055',
			x'a80d73a2c7f7eda5cfda2140fcc1886bdf6fa732247475185d351d8519db4e525009597124fb7ba8f4275738b5eabef1fca00caf7bb023017e979d42a0e1cc2613d72bbbb3850bec1e23e34775f4affb78be66e60162297cc5',
			x'1abe733ea9d4bc8de0593e32d38a228683febb44eb33b22f6cd2205cab5d99aad7d36dfd82edf8175af7d21aaede1124ecf84f0c1024ba87bbf62ca2eb1784b373ed11e5ae0e7ed30acb68a5ba6e2598740c7430100d97e9b81b0c01c103e3c39c8d88d477863160295f19',
			x'fc69fa5c160c9e687f723f038f4cf272664ce6',
			customizer: ";%2nM=I;1Q  FnV^PeMLZb>;h2GbIZ$KU;&#M`~W}=C1--+n?FHR[@qWfv'<ugqTMSI+iT1oeuN>>-.Z(WB#3Oo6IL;VXcb6KnbWb~&j6WlfqS:XhJ5=EnB}"
		)
	);
	/* --> tcId   2 */
	test::eq(x'97ffabaf8584f9c3c01999fcff4c7b5207d40832282a689a6f911e6242f5577e5a745ca609882b9526cca46a331776d39a2d8c216264b9c7dbf91a0e87d9',
		tuple_hash::xof(256, 496 / 8,
			x'993eaf06750fafe46b2efa1d7b81ba819668532e075c',
			customizer: " 9^}&03%1Xv`d:$SQlp*SX;9r7oE2zyX(KA0k-TU)XYm.Y5O5m0X/9kG|E,m?mr6_>W8xrgE8; z~M+8#@0Cj@In_y3F!f6;fD/o#{UB'jAs9pGO;sk~]NJ>"
		)
	);
	/* --> tcId   3 */
	test::eq(x'4cfdc7fd8fe0eae21d9a5fd945df73be582d6e3db0eb96d286665530325153aeef86523c2a9ce05285dc16216d891d',
		tuple_hash::xof(256, 376 / 8,
			x'a6740f48a455a66a0e755df34a98a9e454c6fa90dab923ec96425f55ba2736ad65dbae4d77126b44d332cbb8bd5c0e1d3b8130991ed0e155c2865a109a799dda2e1b4f58b5e465855f1861bc',
			x'90fbfa99b551268ea65ac0692c6081de61c49ffe4c3c1ddfc244123ae0757342c01d8200e6f3772db43577f0a220f97490d07c8afe0a6313fbe08d2d7edbc31d4a638925d512aedd4e9e09af4f18c050729115a492bda54a42571295d0c5cbbde5c60963706b05',
			x'4031520cea1199a19fefd46cb4a8db4b29d8cefd1a70c2dcd51fd8525fea5122afaac0b13ca5f3228bfea2ad6391854a792252193805a901e28fb1dd33259f419c37fe39f3556e7cf20703c371a46b86fd0c77ff2bee5aade4b406ab21e5298ec11352cf188256f1217e0892b28db5e93ad3c35db46155ef0617bb4a46',
			x'8161463bf7d7c32224f46ca84fbc1e8b4682a3a9f09212083329376608ac01dc5e0192a7e12069ba0b9be7c8015bfdf330ab6e051d66c71408285c6091cfbd0bbd6d034cb04e36b488c10fb8a61b30908ac835c6b8be4a624e42f617d5f17b1b538ee431eb87bc657226c368c81656fe',
			x'91501765d8d0961fa03f6db8cdfd3536d4336e5567b4bf7f4303372fa25a27bd39f7d05113abd8f06111c06278cf3e73807b0054e8847e62a22a11b1c1f83160a2e5880c70b6b2d58856dae61661aa54b5f927d3bbcaa2c1044107c5',
			x'4bdb539711377fa79e3f01c8e93491ae10d4b856eb44dbcb5cbe353a8e334e8a75949c732c92c1e45d2e06420ae1226e68dd2a669882bae84f802ad21bdcd6a5820ba90ba3f98c3b3b51d9a52c2b4b86',
			x'f0f8b7b225e9491506a2af89f14383b836c569246c7f1d693080be7ec867c6c0b53b72676109855f7be3f9d515240c7764dcd8c24385fb636019bcaa49c5f9ff77c4adfdbbe7950e88adaa68e27d82ec2b0743c1673e8563392d4ef7180e0c26',
			x'9ef6df4114d53a0e97e4a446040ef50e01caeb92fa762fe5b0600fd771a375c1140ede65f4f39e8cea0395658a9da4c5c9e754ac8e25b17d5bd434b36124112c4a2ee4a9d3bff556777ab7acf48bb9e51370fc82d0bf',
			x'50551aacae00b30b5130b261dcc6dbae87abec703dbb08b30c1f0a05ba5e93e11c734a786c4da14080d6b347fc9f7df97c5511a4a6af439ace86f94f4034e20478ae005abf57039850931f8156d739056f4b3a9d630c901dcd09d06fe6f1d4a057091e35a4ec44b6f275b137cc11a3',
			x'6c6bb39860710a51ae0d622fbb96839cbdc6f5541099f73e49925dee769ab210c492d0',
			customizer: "FrR5v8Z>E0>Wub|A 6l?K8X~{Xu>yHO:F?k7Ar%[-~P%aGw|L58ZNL7/:S9>A=C{_$2Uve .103LfT=S`xqe1tky)]|lYf:nZTD^6A'f5*J>:jGd6d]HK^kj"
		)
	);
	/* --> tcId   4 */
	test::eq(x'0d2fd86ff9596d1be0b39260c486825c9123a56087e2c93a6a1d6128478527ef7f',
		tuple_hash::xof(256, 264 / 8,
			x'3321be4eabd03438dc86791bcd3df5e214ad53f031fe3f52450cbaf40756ddc5218f43a9650118eba66f2c2f2ac31585d387dfb7a9c97e76ca2450c3bbdba709b6a4dee2c2c969f365fcb9b075097696d076cfae5f936900720672cff57ee7601fa238a159e5fba2353e4b12c9f17c3df34542a3',
			x'4bc45c161a92f5b37c50d7a9169eaf569521b240d4cabfa969ff949d1f6197286bfa62d24586a8e020b00f8398a02c2c6d3c613f387ffaae96a19dc37215638bd36f4943cb1d93f661eabfcf79564b06c206b382ed6b97b53d2eefd87d09cadf7f88dae3a7ee',
			x'4ff9bd42b45d0e8cb307c6c7a0e4489628b01b61bf511c88a26d25936315d7368de872e906df399478a12cd47efb4d52e401a76ce54751c2e3c322eef1872ee0db45c3ee362d98d01d00a8101300',
			x'e2dbb7f658e02d519d9294aeb968',
			"",
			customizer: "q;~L(bO#@?W&jb+-#S{`mEvEiEjp[;{pdB_kX E7 9F$oK4G:y`%D|$LDXcaw^E&Bg@H$>S#/d_T~rT=1CVbYwKg>3F~<c?Br@`)+kCzF]u`>NLP$FPS'cJJ"
		)
	);
}

// See: https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-128-1.0/internalProjection.json
//   & https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-256-1.0/internalProjection.json
//
fn void known_vectors_non_xof()
{
	/* tgId 2 / xof: false */
	/* --> tcId  101 */
	test::eq(x"26bae9c50275d9efdead085893f1def8d1c674ad4be636afc1bb8cef9acb124da6c2c361e75c8ca76dd6bf0a9b547ace7beb56c5508653e57e58c1674537",
		tuple_hash::hash(128, 496 / 8,
			x"8e68396630f1ce3725fd507b3eba8028e2a47aaa02425e4eae0170da9bdf9085001b92075d362f1aaf499d7c0dc8f47f8cc18476d7835d206f979d600b7e80c37a5e4433390ce9671b51",
			x"8735548a9f05475b1ebe1af30eb125a5e5518242",
			x"3485dcd7",
			x"64ba92",
			x"0b",
			customizer: "GWz1?q2c-wv$G;bK'o.XGDAb.K8=7MFfF$#Z3PnO8XcS|N]1#kug7B:#68$sXsK_P1[C9(9U ^Am`me_m(:KW_N$i~EzLzuC.%#kq:vzR?fX!LAGQD&PX~(qaWY9b=*&g|`[jxlS0?x8wu#=^|H)uEj."
		)
	);
	/* --> tcId 104 */
	test::eq(x"1d746e263d0e9d0ab3ea9eb1e0b70bbe5fd7748cde392fe18668881057162e67b6b3628a2a431ef357470f7c09",
		tuple_hash::hash(128, 360 / 8,
			x"25f30ad3efe042779e0005b1104c67d101",
			x"8200c248c8638ddc",
			x"a17a080a10e94c14744d392008527db9e8c7",
			x"d519bcc5ae31da14d38d53a7bb663a305e9dbe3b05243e684f559d41041cc0f1a54485b63890498b66b24b58b113a2bc31055c9d0226905acff556f7bc41c71fe605ba5d226c36ba3ac062957f0e5ea64d5a82cc3ba6f65fa1a7226f4b834cc948ca660fb9ef16b3d19843f9863202f264",
			x"1a48c34c1ef0013fdd11814c",
			customizer: "VH?>&R}D'b{T<hrT;8XGJvG/jn8[Pc,0f9<60.ZEQ>X@R`t.8m4)Ci _ 0R6:ArBUkm$L]E.]3t->(Z*y*CbN#l{8[5~YD+1J6ifrpaZH'K1sr@wSH<0}1rS~pkoGER8(db)>s7)Hg:HUZ(D<=J1)y29"
		)
	);
	/* --> tcId 110 */
	test::eq(x"97bf23e6faf6367172fbe01693f1b0449448be0673b493a26a29e59d5caca92f71e8e113dbd9ce91c6a16e642ba0985cb2542711e7c1fd2fe2e2445a75",
		tuple_hash::hash(128, 488 / 8,
			x"63",
			customizer: "yZkGfG]X&J8,N.}<:B|z.h?KPN_N/r.{JF^~wp|iDC-> -[DlZ4S=Q(2<?v7g.I3"
		)
	);
	/* --> tcId 115 */
	test::eq(x"e3823c71cacd3a73e4a243c7678977fa67e79f50c39bbfc419983ff31aaa4e59ecf0d1",
		tuple_hash::hash(128, 280 / 8,
			x"fa2880a7028c8beb7456d83cc1503b076c87ce43",
			x"e4fcaa9fe169bf2d409ac183c0270f9642afc0dff303a6843b9d0281d6a9460f4914e71aac10b8d9605da0f1b5fe47b049507054fbdce5cc1757b099431f8ae08c768108d858e7b204bbbcd8cfbddceb3745a1618ad385ca1dc07394c3bc951c3670af91ea7ef5eab2",
			x"9c4b30c3a9aec879c70dec93d0",
			customizer: "d[g_3M__n x[2>kp25)Un}9oSSKc018U^AkoLma(}(gK=?o[d{]SN-$eCw+^!{N^hC}oQ{CDS!Wg6mQ4dn}3Rc'P>'"
		)
	);
	/* --> tcId 134 */
	test::eq(x"a29affef17899dae989a4879e971252e48133829084e32947549b77842add3dfd33e3193a4ed25a532e195da37ad15b6d40d8f8d9c41659878a5b5003acc35",
		tuple_hash::hash(128, 504 / 8,
			x"984712a8ebca9bfb547321bce4b93fdf1f699a",
			x"3ef86ef36eb14b7c6eb97911c2c8afe5a0d60c482b11692e70e2267259a121a06f1347e576439af3dd264499073b0ea9a9833bf99a5e4886dee43708a428a2b352814cd0715969dbcfa6a774",
			x"20ec639df36f904c7b27830507b3e3f57a7edbc886f9de366a6c9f8d3fc71313ade22ccaab715af7df8616bb3bbcd59cc6d43b0360c0194db70e0e4e6670d391e3bec2dd50a9cab6823b88a047964479bb06fe1259f6d270c07d747527dc3267265d517f3690a3c4e694fa3330d9ca2b83362a99d8f9ef34",
			x"b6cbc6199c1c604d2daeea6ec0dd8ad34bf430ec242560dd82a7eba011f43cc13d53f04c8c60af43ea3185f0cc3ba12804bc6a5ba805aea618537a4c5f4b7eb39eb7aa61068e8fb650d9f1d2cc7830fcf34beb90a8da78e7b5f6db1c16ff1cdc045268fc389474ef4c1236",
			x"e6ad9110f8c7a4090c04e009623fcc1efbb5c6a12728d6d44a25ca6f9416690acf40c4c78dd6ce9312c2b915bb12fd0d608fefb8f93c63aa751fe6c75a",
			x"b3debbb6b4547b118bbfdf761f044d07971d8f9f308353871e4ab7b3f53d964f16a48eab40256fceeac5a74462b22235dffb336e7ac5f9418afcdc242366d73304412842392c02ca8b0a6529c5dd84fa6eda19cc719a913ee7c8d843f0fad84fef6d22",
			x"85c6eb0ecdefb9f9f94e519423414a3c563ad7c11452f4b6056a2bdf5e43a9e109a8f54940334d056749086cbd3c043e38eb0cda59ba537b0f2b421bf5214301ba0fd0287f82286215317c67ce04e247862001a24564f0dad075cb68966237e3b1d6709f848a5e398d513f4a9524f1da93d87e2c05f5d82baf55",
			x"2da635b57706369913588dc93178b0aa",
			x"c088ede1577b1a5e1f887753a4222c4abb94e1b1f37df85a8e101d8d56bf870e3276d543fb66e3c4e17912a007834418fa860c535009ce95cbe9483a40fcd732287ed6e83b26d1c7e731bbc113c443d74b5d8a2df2af8c095f1069cc2ecbf69d3b25644a5a8f7584b4fe03dc12",
			x"56973c884255ebd73740487b58f839accdabf8860d723c360798ead88ae0ca5e6f27c0196ec7a0da9abd6f6cacb9a273f7a0fe96dfd1b216b1ab4fe3308694a7adf434fb6c304ccc41d758",
			customizer: "+t[?aA)x+g//w$t&'PJ}LlQ%vRTvH.1UFo=t^c>v;{h]"
		)
	);

	// Now for 256-bit security
	/* tgId 2 / xof: false */
	/* --> tcId 101 */
	test::eq(x'587399ad4630f49c644c5fae24c81b8a5791fc681feef49f150046c43cc43f640fcfdde68c13a4198c0ca8101b9e83767567c886',
		tuple_hash::hash(256, 416 / 8,
			x'e1ceee94c9830a6e1d1421b1802dd4eb6b',
			x'e313cd01e253a43e6c179cfb27d7145c121565f3be958fc84ebf06eee47b189b907c3e9db22cf297c7b144e695',
			x'f9efd2f9437c8a77e80421d04bbec0cf6818ef6e770a23926525b17bfb4dddb249a50e01ff945466895619dd32d4f3ae56cc920fe8a5e407f1d28d98',
			x'2f2b3c771ade0eb9a06ecdfddad14531355c3e',
			x'59a4e2e4a9afa3ce06046f98d94b81617ec0e6fe23e6b912391134bf6be234f18365d891795a5040',
			x'55cc94cb08195471c6b04667f3dcc6a05e93f8fcb0428c142f5568851c9ddb2f81eb8b8c0f17dc8f5a310756be08a5fd85e002ebc8e94de0',
			x'8a5366ff93bec537c933d7082a740247cf1a8ee4af4e6ba6062512bf82f7f001e79b8ea347c6dcf9835f944dedc07f644d4e30aad72ae919f7aec66fb848975d81c5c9aad84f95ffc4650f986862',
			x'9eb045e147ba5a518dcb39b9d6434d',
			x'1aa100f7c26b',
			customizer: "38g]dGll6db'NZ$%<7z&`Hs=+NFgC|L:JMON=)V`}p AHhEQ_Owu_ypiDJQ6 &I97H[ ,Sf3`;'nHQsB*t)r%VYP]e+rU@`xGf* -DF+id6Dr!AqU0-,|U4`"
		)
	);
	/* --> tcId 107 */
	test::eq(x'ebaa04d6e21bc3b657725da07903803ede1f3fd6c2436f3f42f0bd599c681c2b4e6cb73be3a97dc3f16ba75e83b16589fd8f5c85cd',
		tuple_hash::hash(256, 424 / 8,
			x'57ce2924b5bbe226ec6e8faae0094fffb50da7598b517fb44d8aab98528ec8c526b8464b8fb30722e9fcd80f31dedbfccfe309c6c4fa019e1ab8d7bcdecc23c57ec188bc061ee2bed36b538def996d2546896475544687af754c',
			x'7b22634884e423c2ead10a2e8ddff1687c618165d9a4c857701ad39826bb4053b565de13188fb8176d3a0fa98542f9b08b29d5efb52ba15a357cd83dfb90fdfa7b8f7187907c91515137917afefed448',
			x'28a5f42796ee7563d06e4886d7fe21b013579607616be02dd1deddf2021aaf62763b',
			x'352d4d1b617ff0dd5cae02fe6561b11fef9d',
			customizer: "&2s!,;H fkK`95Gr3y"
		)
	);
	/* --> tcId 109 */
	test::eq(x'dde042a45a92608d3d47fcde091c9c319e1a6d28efd593dc281a86c3c32cc81da00662e4bf1964c55882545368c0df37d062ce8113bbe357d4',
		tuple_hash::hash(256, 456 / 8,
			x'cef1d1',
			x'd46bff82ca',
			x'ca5de9689e7ed4a77c0677b97e03d971',
			x'1c827be062159d5f1947117e3430c36a1762ef7936e145a57d507752',
			x'768a84789e69f04d4b9892221ae29085254b67837922ce67c4c32a9dee62b56b44a1561f49a4e411ef15c904ccf72c2eeeae0a770393e8cdaace',
			x'32ac43e24c25c0aca1161dd92187738ec47e98aa6b9ba6356532680450c0a8c2974f47',
			x'b2de40c57d88cf33',
			x'43a31f91a5be2dc002fcc0fa05be84b2a5622def',
			customizer: "b&; o<RA2XOH6 p:sC~s|)X/v4qOfSoPqn>xj=:oGHx}8]X#(?CI*`bXK-)[J#Z]h~UFawNU*0KS0Zu9YoHG0FF2-dM^5"
		)
	);
	/* --> tcId 114 */
	test::eq(x'6d410850a198abc62c7a30189ab2136782dad76b69b8c626951cd93707c96e86269aaf1d8e2bda980ab9d3c34435816a8b7ac5',
		tuple_hash::hash(256, 408 / 8,
			x'11a4a1dec43e54b616e66146b2a0a0f58b6740281517e8',
			customizer: ":nNj|>2N;KR$|h?E)c1pvb9~~|Q3I/{4JOck+h&M3uzsf^pmIg+SbE&WTOj~k~%,N>XM%R;@]c] fD9A,dha$GQ%I,7Z|&b3qnDG%CiuLju~g&q17Is"
		)
	);
	/* --> tcId 160 */
	test::eq(x'd51945e419a2e9d715e5dc2dd09e35e5f1c45482ea68f4c03372c7b5ab7cc285095321aa50ad9502',
		tuple_hash::hash(256, 320 / 8,
			x'e05fd173abf63b32517ef0e689f87770727d9c80772ef34028db8cff2163784b678ff5ef33e82f87a12b1f71c6075f2b8a6c4d3d066f2dd64f045bdb7ddc5d70efb7eee82302ddbfbba2',
			x'9d',
			x'afed80f2426aac60b7368aa971b1faef7901a48c9cedfa09a7dba3ea162a5408c75cd8ca608697e3e70ffdf80b1a7d60006b070458ec2588b7e179d94d936fb52f96c3ef11fa5a8106fd78c275b2ea20b4f07d85df849f8b4beac5ba07',
			x'0f77c5700bfb30bf8261f5b4f078cfeb0decc8cbe5f5e81b32ecd67144eaf39ed2f179be0983d89e6a91d106de881c0fe5ac2b51eda7388836cff4e49a9b515467d33d0d88b498393db6721e7e1eea26d0710d953e5b7210fe0173a2bd1654ddb04f24c9',
			x'998868b51dc6c0cd8fa78472197d2df7074ef70c50c3c8e906b196c3c970e2299ef2c3d43f57d7e6adc7c0b6ae71c04e77bf3fc0929ba95c5ca80df5ad6e944f5d33e4d9cba5b6460e89a7fa20259113a743315ec03f5c82cc8e79f69ee67055c1e0db40846791a91d0000ee1e76ebd69fabdb1dc35630387368415d4ed447',
			customizer: ".?v7l#Nm7_{l+R1BlmPxI1v&!`G$NN<Rs^%0QlAJou"
		)
	);
}

fn void empty()
{
	test::eq(x"9ac0a836f71e8a54", tuple_hash::hash(128, 8, ""));
	test::eq(x"5e11c1db5e0e0b2ead2dcd343f18b98c", tuple_hash::hash(128, 16, ""));
	test::eq(x"549330469327c593eb95b1d467c48e5781939e135e10632c804ef8a69c73281c", tuple_hash::hash(128, 32, ""));
	test::eq(x"65ce18bce8a9d159bf6d1f49577cda81c57c5532f17fbb54a2d138a6abab08e61bdb64f04e87059c01eb234f1f3de61d94c3f1a1b0636fa3120564befdc1d345", tuple_hash::hash(128, 64, ""));

	test::eq(x"b7ec95826e8c30ae", tuple_hash::hash(256, 8, ""));
	test::eq(x"92c1213a4ba5eddc2a4cb0226ddc132f", tuple_hash::hash(256, 16, ""));
	test::eq(x"98d7c9efb8ca52eb68785cd328682df26a5da9106b3b53d7b87b474955bb66f6", tuple_hash::hash(256, 32, ""));
	test::eq(x"910249ee1253f50db0c195e4b88e9a15a008b2c73ac680aa1825284f04332b5de22c889ec355d0569a8475412169dd0e815f92b33571f418bd38352f130862a6", tuple_hash::hash(256, 64, ""));
}

fn void ordering()
{
	TupleHash{128} h1; h1.init(); h1.update("", ""); char[8] h1_final; h1.final(h1_final[..]);
	test::eq(x"599c788bb322e430", h1_final);

	TupleHash{128} h2; h2.init(); h2.update("", "", ""); char[8] h2_final; h2.final(h2_final[..]);
	test::eq(x"761dd35893c85374", h2_final);

	TupleHash{128} h3; h3.init(); h3.update("", "", "", ""); char[8] h3_final; h3.final(h3_final[..]);
	test::eq(x"c29aa996e58a4c04", h3_final);

	test::ne(tuple_hash::hash(256, 34, "mind", "bending", "hash", "tricks!"), tuple_hash::hash(256, 34, "mindbending", "hashtricks!"));
	test::ne(tuple_hash::hash(256, 32, "mind", "bending", "hash", "tricks!")[..], tuple_hash::hash(256, 64, "mind", "bending", "hash", "tricks!")[:32]);
}

fn void large() @if ($feature(SLOW_TESTS))
{
	const String[][] EXPECTED = {
		{
			x'c576bda1dea5e6f83478420f270aff954de221ccb11cb3f1f192e3aa93f80674',
			x'cba8ae7826a9e53f921879e5321b4dc62ca214489f95c586127dec5a7b7586313e6bf48b23d0a8e5adf1b25cede87fd47e9ab5deea251acc56603dadbff9fde6',
			x'10bf2ae2f4ffe4db4d7943c94ebfc9756f7df8d03fe40aa6e8a782a744406ea1d7e8ee6f612778f57fa54015aa973f1dfdb5df64f61d580314620c0c75dcde77c3a52448ff77342da65cc8cb6dc59e0bbea17359006b25b4940f3a6dd09b429c9c5155ed5f0f6ccc2651eb3ce57f6236f23a5579d38337087da993c645bc036a',
			x'd94ad8fdb24d9e55599ecbb891f9d436f3910504db74317f9fe7b2e6a1179f03',
			x'7885a4b5e911ff33623559f685e97f1388b800a8013c57e03d2292cc26cbd33e2f676a2dfbbbd14c67e6c23cf8eec1bd67030863989f6f7bf47ec75135263fc0',
			x'7fc1f08647b12a64475c5855d10dc9664c9821b36d13086f1bd28f5ebf2d90a03f79488935fa732978edef8ae4ea827a5bb43859ff61ec1fc54cda280e15480de363e009f57c985b7b0c9b099b9af6f2d6c8f91774797b3e07fd73a4b67d309e6523849e9b87133da46be71e7c360751f14b63a3b2d6484de46b84f9df1dd531',
		},
		{
			x'2c8b973d8183407c249fcb837a84b51da5f98acb951daab6c0e999c5a1731c88',
			x'b869b8cf1e1df7db89d3c32a13012f12c3f18a31e96b77c8ab6900e435309b667097b23138cbd2ccbb2fb1a8008c5579d2a386e1309b4f7e294ac09672b8240b',
			x'4ce3afaf34a7c96a6c5fee7dde60bdc4ac7cde5b01c04e333d60e2bf157d192dce4daa912160490f62a0aae7407fc1384f05c8e681b35aafc79ab3a2630354de7c5d3a9fca5f6633b27eed5d040516fc4f0b1493040d21d87d5e967e0db3790e9f25374c955211d249ca3bb9eb40ce2b381132ba2ba8baea7618d7e093a52012',
			x'315dab3e4412729ee63270b7c3d4952bb7f41e4adddfd5a21887bab758226d73',
			x'1cb154d4d249f44232e839c071d831db768c2a34af6e875c02fc1d76ea756e941c845d83d125b9c9110d84073c1ab8db8ccc8f4f973edec847162ae1b8e3353c',
			x'd99d273ca0da7aeae0008d970a384a3cf96e115e4e43a1853701d67848c744efd0e030e2e012b682a9ccc98693185f8da84584569972f96a8b99a175f04f2331d3fe47a944be7a1877124763738c928a3b83c23c777d4e142da89069bae3cc328ffe435d7b21702d8a6d38c68dec5f4be41a5ffcc74b91a8f5c969c753f41f0b',
		},
		{
			x'5bbc6c09f6d1784c88b446dc81146ecd65cc305c9acc82a3ec8444f81872ac9d',
			x'e2fc5a77e0e93d75a59fbcb8ac91a74d4200fadd753f87f32a9e07f974983b66dc2658685aa3833465e4c17cf24a690520e4e1896d49df8075d23653e4292eda',
			x'38ad9f0095e620a773ac7d1b061b44a85d857cf538908ee7132e2fcd90f1e9fe2aeda21c92d85b147c35bf45627c79f756a6725fd40b5518f616327b044feed45ac70fa06662b3422ab6844d47148e6721c7c65c40e8da3d8db629e28bd880a043ac51bdb41850471bcae51fa40825408c914a49f34a9836ce8afa9c15d6998f',
			x'767cfb58c49ff58d2d554d436cfe834e5c469f6b2cd0915a9f528ca16d9deb6c',
			x'd04e005f8cea54368cfd6b7147e0ee8933acf4ba97e2889d8ebebe488c73a861e2e9d71b3b2912f51b4ab6ccbf0f14087f43f478c8a5b8c5f3ceffa96c21ebd6',
			x'113fd02be4d9b1b70074056bd4df006ed5815c6998ad267548f9d90f8ed7e7ba9e9ab4b20284971e96bd59cc43c5e0cd53a7ae00a6290eb1d2b186c0b367619626369e5d369bab88253b4e444dec471467c980a8362fd5dc13e7fb8096af3f5a8f5c2208deca48f2606d950a2f76d5e20f61008bc107149d5cc8134f8d9fa22a',
		},
	};
	char[] bigboi = keccak_test::get_bigboi();
	foreach (i, v : keccak_test::CUSTOMIZATION_STRINGS)
	{
		test::@check(tuple_hash::hash(128, 32,  bigboi, customizer: v) == EXPECTED[i][0]);
		test::@check(tuple_hash::hash(128, 64,  bigboi, customizer: v) == EXPECTED[i][1]);
		test::@check(tuple_hash::hash(128, 128, bigboi, customizer: v) == EXPECTED[i][2]);
		test::@check(tuple_hash::hash(256, 32,  bigboi, customizer: v) == EXPECTED[i][3]);
		test::@check(tuple_hash::hash(256, 64,  bigboi, customizer: v) == EXPECTED[i][4]);
		test::@check(tuple_hash::hash(256, 128, bigboi, customizer: v) == EXPECTED[i][5]);
	}
}

fn void successive_squeezes()
{
	const char[][] EXPECTED = {
		x'3c0b1c55848bfa76d36ead1428366fd8ee78d80f8aa8b709e7d8d063e61fe098',
		x'5387e05c322ad6a3be689b6c84cb3e1eb9ddca8d027e17525007d5d02707a9a5090d6b0d2a4bbb9d454b3c323dc0b45877aab958b05676f64387709add97968c',
	};
	$foreach $i, $sz : cshake::USABLE_LEVELS:
	{
		char[$sz >> 2] buf;
		TupleHashXOF{$sz} t;
		t.init();
		t.update("C3");
		t.squeeze(buf[0:1]);
		t.squeeze(buf[1:5]);
		t.squeeze(buf[6:16]);
		t.squeeze(buf[22..]);
		test::@check(buf[..] == EXPECTED[$i], "Test %d: Got '%h' but expected '%h'.", $i, buf[..], EXPECTED[$i]);
	}
	$endforeach
}
