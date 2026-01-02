// Copyright (c) 2025-2026 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module sha3::tests_common;

// Should be common to every SHA-3 test module. See 'generator.py'
const String[] SIMPLE_VECTORS =
{
	"123",
	"abcdefghijklmnopqrstuvwxyz",
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ",
	"The quick brown fox jumps over the lazy dog",
	"C3 is a great and acquirable language. I recommend it to everyone I know who is doing"
		" systems programming and could use something fresh that doesn't force them into"
		" strange or otherworldly syntax, just for the sake of pretentiousness.",
};

const String[] CUSTOMIZATION_STRINGS =
{
	"",
	"email signature",
	"my customization string that is a bit longer",
};

const String[] KMAC_KEYS =
{
	"correct horse battery staple cryptographic brainrot",
	"RJ5r5ToA9N9~Y-,kin8y]xB}kviaDk_kPI",
};

const String STREAM_VECTOR =
	"This Standard specifies the Secure Hash Algorithm-3 (SHA-3) family of"
	" functions on binary data. Each of the SHA-3 functions is based on an instance"
	" of the KECCAK algorithm that NIST selected as the winner of the SHA-3"
	" Cryptographic Hash Algorithm Competition. This Standard also specifies the"
	" KECCAK-p family of mathematical permutations, including the permutation that"
	" underlies KECCAK, in order to facilitate the development of additional"
	" permutation-based cryptographic functions. The SHA-3 family consists of four"
	" cryptographic hash functions, called SHA3-224, SHA3-256, SHA3-384, and SHA3-512,"
	" and two extendable-output functions (XOFs), called SHAKE128 and SHAKE256. Hash"
	" functions are components for many important information security applications,"
	" including 1) the generation and verification of digital signatures, 2) key"
	" derivation, and 3) pseudorandom bit generation. The hash functions specified in"
	" this Standard supplement the SHA-1 hash function and the SHA-2 family of hash"
	" functions that are specified in FIPS 180-4, the Secure Hash Standard."
	" Extendable-output functions are different from hash functions, but it is possible"
	" to use them in similar ways, with the flexibility to be adapted directly to the"
	" requirements of individual applications, subject to additional security considerations.";

macro get_bigboi()
{
	// 100 million 'a' characters
	char[] bigboi = mem::talloc_array(char, 1_000_000);
	bigboi[..] = 'a';
	return bigboi;
}
