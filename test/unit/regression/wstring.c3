module std::string::wstring_test;
import std;

WString a = @wstring("Hellø");
Char16[] x = @char16("Är du där?");
Char32[] x2 = @char32("你好");

fn void sanity_checks() @test
{
	test::eq(a[:6], (Char16[]) { 'H', 'e', 'l', 'l', 'ø', 0 });
	test::eq(x, "Är du där?".to_temp_utf16()!!);
	test::eq(x2, "你好".to_temp_utf32()!!);
}

fn void test_get_value() @test
{
	test::eq((ushort)'Ä', $$wstr16("ÅÄÖ")[1]);
	const uint VAL = $$wstr32("XYZ👍ABC")[3];
	test::eq((uint)'👍', VAL);
}

fn void lengths() @test
{
	test::eq($$wstr16("ABC").len, 4);
	test::eq($$wstr16("ABC", false).len, 3);
	Char16[?] w = $$wstr16("ÅÄÖ");
	test::eq(w, (Char16[?]) { 'Å', 'Ä', 'Ö', 0 });
}