module std::core::ascii @test;
import std;

fn void test_all()
{
	long x = 0;
	Crc64 check;
	check.init();
	for (char c = 0; c < 255; c++)
	{
		check.update_byte(c);
		if (c.is_upper()) check.update_byte(1);
		if (c.is_lower()) check.update_byte(2);
		if (c.is_alpha()) check.update_byte(4);
		if (c.is_bdigit()) check.update_byte(8);
		if (c.is_odigit()) check.update_byte(16);
		if (c.is_xdigit()) check.update_byte(16);
		if (c.is_digit()) check.update_byte(32);
		if (c.is_graph()) check.update_byte(64);
		check.update_byte(128);
		if (c.is_punct()) check.update_byte(1);
		if (c.is_cntrl()) check.update_byte(2);
		if (c.is_space()) check.update_byte(4);
		check.update_byte(c.to_upper());
		check.update_byte(c.to_lower());
	}
	test::eq(check.final(), 7327699757963224526UL);
}

const AsciiCharset ALPHANUMERIC_WHITESPACE
	= @combine_sets(ALPHA_UPPER_SET, ALPHA_LOWER_SET, NUMBER_SET, WHITESPACE_SET);

fn void asciicharset_contains()
{
	$assert !ALPHANUMERIC_WHITESPACE.@contains('.');
	$assert ALPHANUMERIC_WHITESPACE.@contains('T');
	$assert ALPHANUMERIC_WHITESPACE.@contains('t');
	$assert ALPHANUMERIC_WHITESPACE.@contains('8');
	$assert ALPHANUMERIC_WHITESPACE.@contains(' ');

	test::@check(!ALPHANUMERIC_WHITESPACE.contains('.'));
	test::@check(ALPHANUMERIC_WHITESPACE.contains('T'));
	test::@check(ALPHANUMERIC_WHITESPACE.contains('t'));
	test::@check(ALPHANUMERIC_WHITESPACE.contains('8'));
	test::@check(ALPHANUMERIC_WHITESPACE.contains(' '));
}

fn void asciicharset_create_set()
{
	var $s = @create_set("abc");
	$foreach $c : "abc": $assert $s.@contains($c); $endforeach

	AsciiCharset xyz = create_set("xyz");
	foreach (c : "xyz") test::@check(xyz.contains(c));
}

fn void asciicharset_combine_sets()
{
	var $a_set = @combine_sets(@create_set("123"), @create_set("89"), @create_set("!@#$"), @create_set("something"));
	$foreach $c : "something@123!": $assert $a_set.@contains($c); $endforeach

	AsciiCharset a_set = combine_sets(create_set("123"), create_set("89"), create_set("!@#$"), create_set("something"));
	foreach (c : "something@123!") test::@check(a_set.contains(c));

	// Mixture of runtime and compile-time sets.
	AsciiCharset mixed_set = combine_sets($a_set, a_set, @create_set("456"));
	foreach (c : "something@123!456") test::@check(mixed_set.contains(c));
}
