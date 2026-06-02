module string_to_int;
import std::io, std::math;

const N = 2560;
macro String[] m() @const
{
	String[N] $strings = {};
	$foreach $i : math::iota(int[N]):
		$strings[$i] = @sprintf("%s", $i);
	$endforeach
	return $strings[..];
}

macro String[] m2() @const
{
	String[N] $strings = {};
	$foreach $i : math::iota(int[N]):
		$strings[$i] = @sprintf("%s", -$i);
	$endforeach
	return $strings;
}

fn void string_to_char() @test
{
	int valid = 0;
	foreach (str : m())
	{
		char? x = string::to_integer{char}(str);
		if (try x) valid++;
	}
	assert(valid == 256);
}
fn void string_to_ichar() @test
{
	int valid = 0;
	foreach (str : m2())
	{
		ichar? x = string::to_integer{ichar}(str);
		if (try x) valid++;
	}
	assert(valid == 129);
}

fn void to_integer_accepts_trailing_blanks() @test
{
	assert("123 ".to_long()!! == 123);
	assert("123\t".to_long()!! == 123);
	assert("  -123 \t".to_long()!! == -123);
}

fn void to_integer_rejects_non_digit_chars_in_hex_base() @test
{
	String[*] non_digits = { ":", ";", "<", "=", ">", "?", "@" };
	foreach (s : non_digits)
	{
		assert(@catch(string::to_integer{int}(s, 16)), "base 16: '%s' must be rejected as non-digit", s);
		assert(@catch(string::to_integer{int}(s, 11)), "base 11: '%s' must be rejected as non-digit", s);
		assert(@catch(string::to_integer{int}(s, 10)), "base 10: '%s' must be rejected as non-digit", s);
	}

	assert(@catch(string::to_integer{int}(":F", 16)));
	assert(@catch(string::to_integer{int}("::", 16)));
	assert(@catch(string::to_integer{int}("A:F", 16)));
	assert(@catch(string::to_integer{int}("<5", 16)));

	assert(string::to_integer{int}("AB", 16)!! == 0xAB);
	assert(string::to_integer{int}("ff", 16)!! == 0xFF);
	assert(string::to_integer{int}("0xAF")!! == 0xAF);
	assert(string::to_integer{int}("10", 16)!! == 0x10);
	assert(string::to_integer{int}("9", 16)!! == 9);
	assert(string::to_integer{int}("A", 16)!! == 10);
}
