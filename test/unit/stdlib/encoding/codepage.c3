// Copyright (c) 2026 Koni Marti. All rights reserved.
// Use of this source code is governed by the MIT license.
module std::encoding::codepage_test;
import std::encoding::codepage;

fn void test_cp437() @test
{
	String want = "╔══════════════════════════════════════╗"
                      "║  SYSTEM STATUS: OK - 25°C ± 2°C      ║"
                      "║  Café Menu: Crème Brûlée .... £5.00  ║"
                      "╚══════════════════════════════════════╝";

	char[] bytes =
	x"C9CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDBB"
	x"BA202053595354454D205354415455533A204F4B202D203235F84320F12032F843202020202020BA"
	x"BA202043616682204D656E753A2043728A6D65204272966C8265202E2E2E2E209C352E30302020BA"
	x"C8CDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDBC";

	@pool()
	{
		String got = codepage::decode(tmem, bytes, codepage::charset("cp437"))!!;
		assert(got == want, "cp437 decoding failed: got=%s, want=%s", got, want);

		got = (String)codepage::encode(tmem, got[..], codepage::charset("cp437"))!!;
		assert(got == (String)bytes, "cp437 encoding failed: got=%s, want=%s",
			got, (String)bytes);

	};
}

fn void test_cp437_roundtrip() @test
{
    String s = "╔══ CP437: Café, π≈3.14 ══╗";
    check_roundtrip(s, "cp437");
}

fn void test_cp850_roundtrip() @test
{
    String s = "CP850: Crème Brûlée, Frühstück, £10.50";
    check_roundtrip(s, "cp850");
}

fn void test_cp866_roundtrip() @test
{
    String s = "CP866: Привет мир!";
    check_roundtrip(s, "cp866");
}

fn void test_cp863_roundtrip() @test
{
    String s = "CP863: Québec, érable, Noël";
    check_roundtrip(s, "cp863");
}

fn void test_iso8859_1() @test
{
    String s = "ISO-8859-1: Café, Ångström, Straße, Noël, £10.50";
    check_roundtrip(s, "iso-8859-1");
}

fn void test_iso8859_2_polish() @test
{
    String s = "Polski: Zażółć gęślą jaźń";
    check_roundtrip(s, "iso-8859-2");
}

fn void test_iso8859_2_czech() @test
{
    String s = "Česky: Příliš žluťoučký kůň úpěl ďábelské ódy";
    check_roundtrip(s, "iso-8859-2");
}

fn void test_iso8859_2_hungarian() @test
{
    String s = "Magyar: Árvíztűrő tükörfúrógép";
    check_roundtrip(s, "iso-8859-2");
}

fn void check_roundtrip(String utf8, String charset) => @pool()
{
	CodePageTable *table = codepage::charset(charset)!!;

	// Encode UTF‑8 to code page.
	char[] bytes = codepage::encode(tmem, utf8[..], table)!!;

	// Decode back to UTF‑8 and compare.
	String got = codepage::decode(tmem, bytes, table)!!;

	assert(got == utf8,
		"roundtrip failed: got=%s, want=%s", got, utf8);
}

