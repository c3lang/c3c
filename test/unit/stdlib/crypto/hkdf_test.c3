module std::crypto::hkdf_test @test;
import std::crypto::hkdf, std::hash::sha1, std::hash::sha256;

alias sha256_hkdf_extract = hkdf::extract{Sha256, 32, 64};
alias sha256_hkdf_expand = hkdf::expand{Sha256, 32, 64};
alias sha256_hkdf = hkdf::derive{Sha256, 32, 64};

alias sha1_hkdf_extract = hkdf::extract{Sha1, 20, 64};
alias sha1_hkdf_expand = hkdf::expand{Sha1, 20, 64};
alias sha1_hkdf = hkdf::derive{Sha1, 20, 64};

// Inputs shared between RFC 5869 test cases - https://www.rfc-editor.org/info/rfc5869/#appendix-A
const char[*] SALT_13   = x"000102030405060708090a0b0c";
const char[*] INFO_10   = x"f0f1f2f3f4f5f6f7f8f9";
const char[*] IKM_22_0B = x"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b";
const char[*] IKM_80    = x"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f";
const char[*] SALT_80   = x"606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf";
const char[*] INFO_80   = x"b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";

// RFC 5869 Appendix A.1: basic, SHA-256
fn void hkdf_rfc5869_a1_sha256() @test
{
	char[*] ikm          = IKM_22_0B;
	char[*] salt         = SALT_13;
	char[*] info         = INFO_10;
	char[*] expected_prk = x"077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5";
	char[*] expected_okm = x"3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865";

	char[32] prk = sha256_hkdf_extract(ikm[..], salt[..]);
	assert(prk == expected_prk, "PRK mismatch");

	char[42] okm;
	sha256_hkdf_expand(prk[..], info[..], okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[42] okm_derived;
	sha256_hkdf(ikm[..], salt[..], info[..], okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.2: longer inputs/outputs, SHA-256
fn void hkdf_rfc5869_a2_sha256_long() @test
{
	char[*] ikm          = IKM_80;
	char[*] salt         = SALT_80;
	char[*] info         = INFO_80;
	char[*] expected_prk = x"06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244";
	char[*] expected_okm = x"b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87";

	char[32] prk = sha256_hkdf_extract(ikm[..], salt[..]);
	assert(prk == expected_prk, "PRK mismatch");

	char[82] okm;
	sha256_hkdf_expand(prk[..], info[..], okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[82] okm_derived;
	sha256_hkdf(ikm[..], salt[..], info[..], okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.3: zero-length salt/info, SHA-256
fn void hkdf_rfc5869_a3_sha256_empty_salt_info() @test
{
	char[*] ikm          = IKM_22_0B;
	char[*] expected_prk = x"19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04";
	char[*] expected_okm = x"8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8";
	char[] empty;

	char[32] prk = sha256_hkdf_extract(ikm[..], empty);
	assert(prk == expected_prk, "PRK mismatch");
	assert(sha256_hkdf_extract(ikm[..]) == expected_prk, "default-salt PRK mismatch");

	char[42] okm;
	sha256_hkdf_expand(prk[..], empty, okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[42] okm_derived;
	sha256_hkdf(ikm[..], empty, empty, okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.4: basic, SHA-1
fn void hkdf_rfc5869_a4_sha1() @test
{
	char[*] ikm          = x"0b0b0b0b0b0b0b0b0b0b0b";
	char[*] salt         = SALT_13;
	char[*] info         = INFO_10;
	char[*] expected_prk = x"9b6c18c432a7bf8f0e71c8eb88f4b30baa2ba243";
	char[*] expected_okm = x"085a01ea1b10f36933068b56efa5ad81a4f14b822f5b091568a9cdd4f155fda2c22e422478d305f3f896";

	char[20] prk = sha1_hkdf_extract(ikm[..], salt[..]);
	assert(prk == expected_prk, "PRK mismatch");

	char[42] okm;
	sha1_hkdf_expand(prk[..], info[..], okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[42] okm_derived;
	sha1_hkdf(ikm[..], salt[..], info[..], okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.5: longer inputs/outputs, SHA-1
fn void hkdf_rfc5869_a5_sha1_long() @test
{
	char[*] ikm          = IKM_80;
	char[*] salt         = SALT_80;
	char[*] info         = INFO_80;
	char[*] expected_prk = x"8adae09a2a307059478d309b26c4115a224cfaf6";
	char[*] expected_okm = x"0bd770a74d1160f7c9f12cd5912a06ebff6adcae899d92191fe4305673ba2ffe8fa3f1a4e5ad79f3f334b3b202b2173c486ea37ce3d397ed034c7f9dfeb15c5e927336d0441f4c4300e2cff0d0900b52d3b4";

	char[20] prk = sha1_hkdf_extract(ikm[..], salt[..]);
	assert(prk == expected_prk, "PRK mismatch");

	char[82] okm;
	sha1_hkdf_expand(prk[..], info[..], okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[82] okm_derived;
	sha1_hkdf(ikm[..], salt[..], info[..], okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.6: zero-length salt/info, SHA-1
fn void hkdf_rfc5869_a6_sha1_empty_salt_info() @test
{
	char[*] ikm          = IKM_22_0B;
	char[*] expected_prk = x"da8c8a73c7fa77288ec6f5e7c297786aa0d32d01";
	char[*] expected_okm = x"0ac1af7002b3d761d1e55298da9d0506b9ae52057220a306e07b6b87e8df21d0ea00033de03984d34918";
	char[] empty;

	char[20] prk = sha1_hkdf_extract(ikm[..], empty);
	assert(prk == expected_prk, "PRK mismatch");

	char[42] okm;
	sha1_hkdf_expand(prk[..], empty, okm[..]);
	assert(okm == expected_okm, "OKM mismatch");

	char[42] okm_derived;
	sha1_hkdf(ikm[..], empty, empty, okm_derived[..]);
	assert(okm_derived == expected_okm, "derive mismatch");
}

// A.7: salt not provided (defaults to HashLen zeros), SHA-1
fn void hkdf_rfc5869_a7_sha1_salt_not_provided() @test
{
	char[*] ikm          = x"0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c";
	char[*] expected_prk = x"2adccada18779e7c2077ad2eb19d3f3e731385dd";
	char[*] expected_okm = x"2c91117204d745f3500d636a62f64f0ab3bae548aa53d423b0d1f27ebba6f5e5673a081d70cce7acfc48";
	char[] empty;

	char[20] prk = sha1_hkdf_extract(ikm[..]);
	assert(prk == expected_prk, "PRK mismatch");

	char[42] okm;
	sha1_hkdf_expand(prk[..], empty, okm[..]);
	assert(okm == expected_okm, "OKM mismatch");
}
