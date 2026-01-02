// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module sha3::tuple_hash_tests @test;

import sha3::tests_common;
import std::hash::sha3::tuple_hash;


fn void output_length_changes_hash_but_not_xof()
{
	// this doesn't appear in the test-suite, but notice that varying out-length values for the _hash_ subset of tuple_hash functions
	//   will NOT produce substrings of an extensible output like the XOF variants do
	test::ne(
		tuple_hash::hash(128, 496 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32],
		tuple_hash::hash(128, 472 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32]
	);

	// this doesn't appear in the test suite, but proves that the _xof_ subset is just using an underlying
	//   EXTENDED output primitive (keccak output is not affected by out-length)
	test::eq(
		tuple_hash::xof(128, 496 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32],
		tuple_hash::xof(128, 472 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32]
	);
}

fn void xof_neq_hash()
{
    test::ne(
		tuple_hash::hash(128, 496 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32],
		tuple_hash::xof (128, 496 / 8, x"abcd", x"efef", optional_customization: "bbb123123___A")[:32]
	);
}

// See: https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-128-1.0/internalProjection.json
//   & https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-256-1.0/internalProjection.json
//
fn void known_vectors_xof()
{
	/* tgId  1  / xof: true */
	/* --> tcId    1 */
	test::eq(x"2a1734392ea30b0e48fc1358dad70daa234ff87477525b80e1be33ec32074eeda4e23870881b",
		tuple_hash::xof(128, 304 / 8,
			x"dc246a7b5fe7dd2e63df4c1310fc30839c65995d2369141dff7af80481630b7b6f4b634c587ffd12f06503a5ee71c7ad150c08ba4bbaa24070c3fa5df9ce9366843b84d7a94590a51fe518c6aa85e0fad7f2d315ca90b7f4230d91e9241cf7082bad2acf6b9ea0e821d24a02aec54d02ac1f79f1d7bf14af0bf076492db27869",
			"",
			"",
			optional_customization: "V{6?N|`G4bJ@:QG?.p^kO81h<]*xNxFk2I7QVhBzlkhf77tz01dG)iI?kM$qsfx;<^.o;uhEMc}cNJP%g.i=4}bH ve)eP('rMJW)&`9H?yEjeo@{}+e|Z'={eoC0`_|[wp! x{bMOtt%<]:(D-fEA*7"
		)
	);
}

// See: https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-128-1.0/internalProjection.json
//   & https://github.com/usnistgov/ACVP-Server/blob/master/gen-val/json-files/TupleHash-256-1.0/internalProjection.json
//
fn void known_vectors_non_xof()
{
	/* tgId  2 / xof: false */
	/* --> tcId  101 */
	test::eq(x"26bae9c50275d9efdead085893f1def8d1c674ad4be636afc1bb8cef9acb124da6c2c361e75c8ca76dd6bf0a9b547ace7beb56c5508653e57e58c1674537",
		tuple_hash::hash(128, 496 / 8,
			x"8e68396630f1ce3725fd507b3eba8028e2a47aaa02425e4eae0170da9bdf9085001b92075d362f1aaf499d7c0dc8f47f8cc18476d7835d206f979d600b7e80c37a5e4433390ce9671b51",
			x"8735548a9f05475b1ebe1af30eb125a5e5518242",
			x"3485dcd7",
			x"64ba92",
			x"0b",
			optional_customization: "GWz1?q2c-wv$G;bK'o.XGDAb.K8=7MFfF$#Z3PnO8XcS|N]1#kug7B:#68$sXsK_P1[C9(9U ^Am`me_m(:KW_N$i~EzLzuC.%#kq:vzR?fX!LAGQD&PX~(qaWY9b=*&g|`[jxlS0?x8wu#=^|H)uEj."
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
			optional_customization: "VH?>&R}D'b{T<hrT;8XGJvG/jn8[Pc,0f9<60.ZEQ>X@R`t.8m4)Ci _ 0R6:ArBUkm$L]E.]3t->(Z*y*CbN#l{8[5~YD+1J6ifrpaZH'K1sr@wSH<0}1rS~pkoGER8(db)>s7)Hg:HUZ(D<=J1)y29"
		)
	);
	/* --> tcId 110 */
	test::eq(x"97bf23e6faf6367172fbe01693f1b0449448be0673b493a26a29e59d5caca92f71e8e113dbd9ce91c6a16e642ba0985cb2542711e7c1fd2fe2e2445a75",
		tuple_hash::hash(128, 488 / 8,
			x"63",
			optional_customization: "yZkGfG]X&J8,N.}<:B|z.h?KPN_N/r.{JF^~wp|iDC-> -[DlZ4S=Q(2<?v7g.I3"
		)
	);
	/* --> tcId 115 */
	test::eq(x"e3823c71cacd3a73e4a243c7678977fa67e79f50c39bbfc419983ff31aaa4e59ecf0d1",
		tuple_hash::hash(128, 280 / 8,
			x"fa2880a7028c8beb7456d83cc1503b076c87ce43",
			x"e4fcaa9fe169bf2d409ac183c0270f9642afc0dff303a6843b9d0281d6a9460f4914e71aac10b8d9605da0f1b5fe47b049507054fbdce5cc1757b099431f8ae08c768108d858e7b204bbbcd8cfbddceb3745a1618ad385ca1dc07394c3bc951c3670af91ea7ef5eab2",
			x"9c4b30c3a9aec879c70dec93d0",
			optional_customization: "d[g_3M__n x[2>kp25)Un}9oSSKc018U^AkoLma(}(gK=?o[d{]SN-$eCw+^!{N^hC}oQ{CDS!Wg6mQ4dn}3Rc'P>'"
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
			optional_customization: "+t[?aA)x+g//w$t&'PJ}LlQ%vRTvH.1UFo=t^c>v;{h]"
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
