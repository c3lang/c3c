// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module argon2_test @test;

import std::crypto::argon2;
import std::collections::pair, std::encoding::base64;


const char[] RFC_PASSWORD = x'0101010101010101010101010101010101010101010101010101010101010101';
const char[] RFC_SALT = x'02020202020202020202020202020202';
const char[] RFC_SECRET = x'0303030303030303';
const char[] RFC_ASSOC = x'040404040404040404040404';

const Argon2Options RFC_OPTIONS = {
	.salt = RFC_SALT,
	.key = RFC_SECRET,
	.assoc = RFC_ASSOC,
	.lanes = 4,
	.mem_size = 32,
	.iterations = 3,
};

fn void rfc_9106__sec_5_1()
{
	const char[] EXPECTED = x'512b391b6f1162975371d30919734294f868e3be3984f3c1a13a4db9fabe4acb';

	char[] result = mem::talloc_array(char, EXPECTED.len);
	argon2::thash_raw(result, RFC_PASSWORD, { ...RFC_OPTIONS, .algorithm = ARGON2_D });

	test::@check(result == EXPECTED);
}

fn void rfc_9106__sec_5_2()
{
	const char[] EXPECTED = x'c814d9d1dc7f37aa13f0d77f2494bda1c8de6b016dd388d29952a4c4672b6ce8';

	char[] result = mem::talloc_array(char, EXPECTED.len);
	argon2::thash_raw(result, RFC_PASSWORD, { ...RFC_OPTIONS, .algorithm = ARGON2_I });

	test::@check(result == EXPECTED);
}

fn void rfc_9106__sec_5_3()
{
	const char[] EXPECTED = x'0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659';

	char[] result = mem::talloc_array(char, EXPECTED.len);
	argon2::thash_raw(result, RFC_PASSWORD, { ...RFC_OPTIONS, .algorithm = ARGON2_ID });

	test::@check(result == EXPECTED);
}

fn void simple_verify()
{
	String ret = argon2::thash(RFC_PASSWORD, { ...RFC_OPTIONS, .algorithm = ARGON2_D });
	test::@check(argon2::tverify(ret, RFC_PASSWORD, key: RFC_SECRET)!! == true);
}

fn void many_lanes()
{
	const String EXPECTED = "$argon2id$v=19$m=2048,t=3,p=256$MTIzNDEyMzQ$c333ffXMw/Om2Z6QIozFEyElORODnvyWzspNjCb0y+w";
	String result = argon2::thash(RFC_PASSWORD, { .iterations = 3, .salt = "12341234", .key = {}, .assoc = {}, .mem_size = 8*256, .lanes = 256, .algorithm = ARGON2_ID });
	test::eq(result, EXPECTED);
}

fn void hash_and_verify_leak_check()
{
	String ret = argon2::hash(mem, RFC_PASSWORD, { ...RFC_OPTIONS, .algorithm = ARGON2_D });
	defer mem::free(ret);

	test::@check(argon2::verify(mem, ret, RFC_PASSWORD,  RFC_SECRET)!! == true);
}

fn void encode_and_decode_leak_check()
{
	Argon2Options opts = {
		.algorithm = ARGON2_ID,
		.salt = x'8198 95fc cd60 3dcd b612 5007 fc98 751f',
		.mem_size = 32,
		.lanes = 1,
		.iterations = 2,
	};
	String result = argon2::hash(mem, "hunter2", opts);
	defer mem::free(result);

	Argon2Decoded d = argon2::decode(mem, result)!!;
	defer d.free();

	test::@check(d.opts == opts);
}

// See: https://github.com/P-H-C/phc-string-format/blob/e8fbd333dcc9a8b0843fac6b33371cf157e91a48/phc-sf-spec.md
fn void phc_string_format__encode()
{
	const String EXPECTED =   "$argon2id$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno";
	const String EXPECTED_2 = "$argon2id$v=19$m=65536,t=2,p=1,keyid=abase64value$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno";

	Argon2Options opts = {
		.algorithm = ARGON2_ID,
		.key = "pepper",
		.salt = x'8198 95fc cd60 3dcd b612 5007 fc98 751f',
		.mem_size = 65536,
		.lanes = 1,
		.iterations = 2,
	};

	String result = argon2::thash("hunter2", opts);
	test::@check(result == EXPECTED, "Expected '%s'; got '%s'.", EXPECTED, result);

	String result_2 = argon2::thash("hunter2", opts, key_id: base64::tdecode("abase64value", padding: base64::NO_PAD)!!);
	test::@check(result_2 == EXPECTED_2, "Expected '%s'; got '%s'.", EXPECTED_2, result_2);
}

fn void phc_string_format__decode()
{
	const char[] INPUT = "$argon2id$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno";

	const Argon2Options EXPECTED_OPTS = {
		.algorithm = ARGON2_ID,
		// .key = "pepper",   // this information is not conveyed in the encoded hash string, since it's a secret key
		.salt = x'8198 95fc cd60 3dcd b612 5007 fc98 751f',
		.mem_size = 65536,
		.lanes = 1,
		.iterations = 2,
	};
	const char[] EXPECTED_HASH = "";

	Argon2Decoded c = argon2::tdecode(INPUT)!!;
	test::eq(c.opts, EXPECTED_OPTS);
}

fn void phc_string_format__decode_errors()
{
	const Pair{ String, fault }[] FAULT_MAP = {
		// Garbage
		{ "$123.50 and a sunny afternoon", argon2::INVALID_FORMAT },
		// Missing leading $
		{ "argon2id$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Missing '$' between hash and parameters, so not enough tokens (minimum is 4)
		{ "$argon2id$v=19$m=65536,t=2,p=1CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Too many '$'
		{ "$$$$$$$$$argon2id$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// One of the tokens is too short (< 3 chars shouldn't be possible)
		{ "$argon2id$OK$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Invalid algorithm (garbage)
		{ "$hmac$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_ALGORITHM },
		// Invalid algorithm (case-sensitive)
		{ "$ARGON2ID$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_ALGORITHM },
		// Bad version key (MUST be `v=`)
		{ "$argon2id$version=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_VERSION },
		// Bad version, must be a decimal and numeric value
		{ "$argon2id$v=0x13$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_VERSION },
		{ "$argon2id$v=thebest$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_VERSION },
		// Not enough options, even if all key names are legit
		{ "$argon2id$v=19$m=65536,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::NOT_ENOUGH_PARAMETERS },
		// Duplicate parameter
		{ "$argon2id$v=19$m=65536,t=2,p=1,m=64$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::DUPLICATE_PARAMETERS },
		// One of the options is actually two short (MUST be >= 3 chars; 'x=y')
		{ "$argon2id$v=19$m=,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Missing option value entirely (dangling `=`)
		{ "$argon2id$v=19$m=65536,t=2,p=1,data=$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Empty key name encountered
		{ "$argon2id$v=19$m=65536,=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_FORMAT },
		// Bad parameter value (because `=` is not part of the value charset)
		{ "$argon2id$v=19$m=65536,t=2,p=1,data=123=456$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::BAD_PARAMETER_VALUE },
		// Some invalid parameters (mem_size, lanes, iterations, and base64 values)
		{ "$argon2id$v=19$m=hithere,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_MEM_SIZE },
		{ "$argon2id$v=19$m=65536,t=0x456,p=1$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_ITERATIONS },
		{ "$argon2id$v=19$m=65536,t=2,p=1.23$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_PARALLELISM },
		{ "$argon2id$v=19$m=65536,t=2,p=1,keyid=-1230834$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_KEY_ID },
		{ "$argon2id$v=19$m=65536,t=2,p=1,data=-notbase64$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_ASSOC },
		// Missing required parameters (m,t,p)
		{ "$argon2id$v=19$t=2,p=1,data=gZiV/M1gPc22ElAH/Jh1Hw$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::MISSING_MEM_SIZE },
		{ "$argon2id$v=19$m=65536,p=1,data=gZiV/M1gPc22ElAH/Jh1Hw$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::MISSING_ITERATIONS },
		{ "$argon2id$v=19$m=65536,t=2,data=gZiV/M1gPc22ElAH/Jh1Hw$gZiV/M1gPc22ElAH/Jh1Hw$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::MISSING_PARALLELISM },
		// Invalid salt or hash base64
		{ "$argon2id$v=19$m=65536,t=2,p=1$**thisisnotbase64!**$CWOrkoo7oJBQ/iyh7uJ0LO2aLEfrHwTWllSAxT0zRno", argon2::INVALID_SALT_ENCODING },
		{ "$argon2id$v=19$m=65536,t=2,p=1$gZiV/M1gPc22ElAH/Jh1Hw$**thisisnotbase64!**", argon2::INVALID_HASH_ENCODING },
	};
	foreach (i, x : FAULT_MAP)
	{
		if (catch err = argon2::tdecode(x.first))
		{
			test::@check(err == x.second, "Test %d: Failed to get the expected fault type; %s != %s", i, err, x.second);
		}
		else
		{
			test::@check(false, "Test %d: Decoding hash did not fault, but it should have. Expected fault '%s'.", i, x.second);
		}
	}
}
