module std::io;

macro void expect_d($value, $expected, $label)
{
	String s = string::format(mem, "%d", $value);
	defer free(s);
	assert(s == $expected, "got '%s'; want '%s' for %s", s, $expected, $label);
}

macro void expect_x($value, $expected, $label)
{
	String s = string::format(mem, "%x", $value);
	defer free(s);
	assert(s == $expected, "got '%s'; want '%s' for %s", s, $expected, $label);
}

macro void expect_o($value, $expected, $label)
{
	String s = string::format(mem, "%o", $value);
	defer free(s);
	assert(s == $expected, "got '%s'; want '%s' for %s", s, $expected, $label);
}

macro void expect_b($value, $expected, $label)
{
	String s = string::format(mem, "%b", $value);
	defer free(s);
	assert(s == $expected, "got '%s'; want '%s' for %s", s, $expected, $label);
}

macro void expect_all_bases($value, $d, $x, $o, $b, $label)
{
	expect_d($value, $d, $label);
	expect_x($value, $x, $label);
	expect_o($value, $o, $label);
	expect_b($value, $b, $label);
}

fn void printf_numbers_all_bases_per_type() @test
{
	expect_all_bases((ichar)-42, "-42", "-2a", "-52", "-101010", "ichar");
	expect_all_bases((short)-42, "-42", "-2a", "-52", "-101010", "short");
	expect_all_bases((int)-42, "-42", "-2a", "-52", "-101010", "int");
	expect_all_bases((long)-42, "-42", "-2a", "-52", "-101010", "long");
	expect_all_bases((int128)-42, "-42", "-2a", "-52", "-101010", "int128");
	expect_all_bases((iptr)-42, "-42", "-2a", "-52", "-101010", "iptr");
	expect_all_bases((isz)-42, "-42", "-2a", "-52", "-101010", "isz");

	expect_all_bases((char)42, "42", "2a", "52", "101010", "char");
	expect_all_bases((ushort)42, "42", "2a", "52", "101010", "ushort");
	expect_all_bases((uint)42, "42", "2a", "52", "101010", "uint");
	expect_all_bases((ulong)42, "42", "2a", "52", "101010", "ulong");
	expect_all_bases((uint128)42, "42", "2a", "52", "101010", "uint128");
	expect_all_bases((uptr)42, "42", "2a", "52", "101010", "uptr");
	expect_all_bases((usz)42, "42", "2a", "52", "101010", "usz");
}

fn void printf_numbers_decimal_ranges() @test
{
	expect_d((ichar)-128, "-128", "ichar min");
	expect_d((ichar)127, "127", "ichar max");

	expect_d((short)-32768, "-32768", "short min");
	expect_d((short)32767, "32767", "short max");

	expect_d(int.min, "-2147483648", "int min");
	expect_d(int.max, "2147483647", "int max");

	expect_d(long.min, "-9223372036854775808", "long min");
	expect_d(long.max, "9223372036854775807", "long max");

	expect_d(int128.min, "-170141183460469231731687303715884105728", "int128 min");
	expect_d(int128.max, "170141183460469231731687303715884105727", "int128 max");

	expect_d((char)0, "0", "char min");
	expect_d((char)255, "255", "char max");

	expect_d((ushort)0, "0", "ushort min");
	expect_d(ushort.max, "65535", "ushort max");

	expect_d((uint)0, "0", "uint min");
	expect_d(uint.max, "4294967295", "uint max");

	expect_d((ulong)0, "0", "ulong min");
	expect_d(ulong.max, "18446744073709551615", "ulong max");

	expect_d((uint128)0, "0", "uint128 min");
	expect_d(uint128.max, "340282366920938463463374607431768211455", "uint128 max");

	if (bitsizeof(iptr) == 64)
	{
		expect_d(iptr.min, "-9223372036854775808", "iptr min");
		expect_d(iptr.max, "9223372036854775807", "iptr max");
	}
	else
	{
		expect_d(iptr.min, "-2147483648", "iptr min");
		expect_d(iptr.max, "2147483647", "iptr max");
	}

	if (bitsizeof(isz) == 64)
	{
		expect_d(isz.min, "-9223372036854775808", "isz min");
		expect_d(isz.max, "9223372036854775807", "isz max");
	}
	else
	{
		expect_d(isz.min, "-2147483648", "isz min");
		expect_d(isz.max, "2147483647", "isz max");
	}

	if (bitsizeof(uptr) == 64)
	{
		expect_d((uptr)0, "0", "uptr min");
		expect_d(uptr.max, "18446744073709551615", "uptr max");
	}
	else
	{
		expect_d((uptr)0, "0", "uptr min");
		expect_d(uptr.max, "4294967295", "uptr max");
	}

	if (bitsizeof(usz) == 64)
	{
		expect_d((usz)0, "0", "usz min");
		expect_d(usz.max, "18446744073709551615", "usz max");
	}
	else
	{
		expect_d((usz)0, "0", "usz min");
		expect_d(usz.max, "4294967295", "usz max");
	}

	expect_d(ulong.max, "18446744073709551615", "ulong max fast decimal path");
	expect_d((uint128)ulong.max + 1, "18446744073709551616", "uint128 safe decimal path");
}

fn void printf_numbers_radix_formats() @test
{
	expect_x((char)255, "ff", "char max hex");
	expect_o((char)255, "377", "char max oct");
	expect_b((char)255, "11111111", "char max bin");

	expect_x((ushort)65535, "ffff", "ushort max hex");
	expect_o((ushort)65535, "177777", "ushort max oct");
	expect_b((ushort)65535, "1111111111111111", "ushort max bin");

	expect_x(uint.max, "ffffffff", "uint max hex");
	expect_o(uint.max, "37777777777", "uint max oct");
	expect_b(uint.max, "11111111111111111111111111111111", "uint max bin");

	expect_x(ulong.max, "ffffffffffffffff", "ulong max hex");
	expect_o(ulong.max, "1777777777777777777777", "ulong max oct");

	expect_x(uint128.max, "ffffffffffffffffffffffffffffffff", "uint128 max hex");
	expect_o((uint128)0xFFEEDDCC_BBAA9988_77665544_33221100,
		"3777355671456725231420735462524206310410400",
		"uint128 sample oct");

	expect_x((uint128)0xFFEEDDCC_BBAA9988_77665544_33221100,
		"ffeeddccbbaa99887766554433221100",
		"uint128 sample hex");

	if (bitsizeof(uptr) == 64)
	{
		expect_x(uptr.max, "ffffffffffffffff", "uptr max hex");
	}
	else
	{
		expect_x(uptr.max, "ffffffff", "uptr max hex");
	}

	if (bitsizeof(usz) == 64)
	{
		expect_x(usz.max, "ffffffffffffffff", "usz max hex");
	}
	else
	{
		expect_x(usz.max, "ffffffff", "usz max hex");
	}
}
