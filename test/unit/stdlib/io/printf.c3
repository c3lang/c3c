module std::io @test;

fn void printf_int()
{
	String s;
	s = string::format(mem, "[%-10d]", 78);
	assert(s == "[78        ]");
	free(s);
	s = string::format(mem, "[%10d]", 78);
	assert(s == "[        78]");
	free(s);
	s = string::format(mem, "[%010d]", 78);
	assert(s == "[0000000078]");
	free(s);
	s = string::format(mem, "[%+10d]", 78);
	assert(s == "[       +78]");
	free(s);
	s = string::format(mem, "[%-+10d]", 78);
	assert(s == "[+78       ]");
	free(s);
}

fn void printf_a()
{
	String s;
	s = string::format(mem, "%08.2a", 234.125);
	assert(s == "0x1.d4p+7", "got '%s'; want '0x1.d4p+7'", s);
	free(s);
	s = string::format(mem, "%a", 234.125);
	assert(s == "0x1.d44p+7", "got '%s'; want '0x1.d44p+7'", s);
	free(s);
	s = string::format(mem, "%A", 234.125);
	assert(s == "0X1.D44P+7", "got '%s'; want '0X1.D44P+7'", s);
	free(s);
	s = string::format(mem, "%20a", 234.125);
	assert(s == "          0x1.d44p+7", "got '%s'; want '          0x1.d44p+7'", s);
	free(s);
	s = string::format(mem, "%-20a", 234.125);
	assert(s == "0x1.d44p+7          ", "got '%s'; want '0x1.d44p+7          '", s);
	free(s);
	s = string::format(mem, "%-20s", "hello world");
	assert(s == "hello world         ", "got '%s'; want 'hello world         '", s);
	free(s);
	s = string::format(mem, "%20s", "hello world");
	assert(s == "         hello world", "got '%s'; want '         hello world'", s);
	free(s);

	String str = "hello!";
	s = string::format(mem, "%-20s", str);
	assert(s == "hello!              ", "got '%s'; want 'hello!              '", s);
	free(s);
	s = string::format(mem, "%20s", str);
	assert(s == "              hello!", "got '%s'; want '              hello!'", s);
	free(s);

	int[2] a = { 12, 23 };
	s = string::format(mem, "%-20s", a);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	free(s);
	s = string::format(mem, "%20s", a);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);
	free(s);

	s = string::format(mem, "%-20s", a[..]);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	free(s);
	s = string::format(mem, "%20s", a[..]);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);
	free(s);

	float[2] f = { 12.0, 23.0 };
	s = string::format(mem, "%-24s", f);
	assert(s == "[12.000000, 23.000000]  ", "got '%s'; want '[12.000000, 23.000000]  '", s);
	free(s);
	s = string::format(mem, "%24s", f);
	assert(s == "  [12.000000, 23.000000]", "got '%s'; want '  [12.000000, 23.000000]'", s);
	free(s);

	int[<2>] vec = { 12, 23 };
	s = string::format(mem, "%-20s", vec);
	assert(s == "[<12, 23>]          ", "got '%s'; want '[<12, 23>]          '", s);
	free(s);
	s = string::format(mem, "%20s", vec);
	assert(s == "          [<12, 23>]", "got '%s'; want '          [<12, 23>]'", s);
	free(s);

	String ss = "hello world";
	s = string::format(mem, "%.4s %.5s", ss, ss);
	assert(s == "hell hello", "got '%s'; want 'hell hello'", s);
	free(s);
}

enum PrintfTest : ushort
{
	ENUMA,
	ENUMB,
}

fn void printf_enum()
{
	String s;

	s = string::format(mem, "%s", PrintfTest.ENUMA);
	assert(s == "ENUMA", "got '%s'; want 'ENUMA'", s);
	free(s);
	s = string::format(mem, "%s", PrintfTest.ENUMB);
	assert(s == "ENUMB", "got '%s'; want 'ENUMB'", s);
	free(s);

	s = string::format(mem, "%d", PrintfTest.ENUMA.ordinal);
	assert(s == "0", "got '%s'; want '0'", s);
	free(s);
	s = string::format(mem, "%d", PrintfTest.ENUMB.ordinal);
	assert(s == "1", "got '%s'; want '1'", s);
	free(s);
}

fn void printf_char_utf8_boundaries()
{
	// 0x7F (DEL) — must encode as a single 1-byte ASCII char.
	String s7f = string::tformat("%c", (Char32)0x7F);
	assert(s7f.len == 1, "0x7F should encode as 1 byte, got %d bytes", s7f.len);
	assert(s7f[0] == 0x7F, "0x7F should encode as byte 0x7F, got 0x%X", s7f[0]);
	// Reject the overlong 2-byte form 0xC1 0xBF.
	assert(!(s7f.len == 2 && s7f[0] == 0xC1 && s7f[1] == 0xBF), "0x7F was emitted as overlong UTF-8 (C1 BF)");

	// 0x7FF — must encode as a 2-byte sequence (0xDF 0xBF).
	String s7ff = string::tformat("%c", (Char32)0x7FF);
	assert(s7ff.len == 2, "0x7FF should encode as 2 bytes, got %d bytes", s7ff.len);
	assert(s7ff[0] == 0xDF && s7ff[1] == 0xBF, "0x7FF should encode as DF BF, got %02X %02X", s7ff[0], s7ff[1]);

	// 0xFFFF — must encode as a 3-byte sequence (0xEF 0xBF 0xBF).
	String sffff = string::tformat("%c", (Char32)0xFFFF);
	assert(sffff.len == 3, "0xFFFF should encode as 3 bytes, got %d bytes", sffff.len);
	assert(sffff[0] == 0xEF && sffff[1] == 0xBF && sffff[2] == 0xBF, "0xFFFF should encode as EF BF BF, got %02X %02X %02X",
		sffff[0], sffff[1], sffff[2]);

	String sA = string::tformat("%c", (Char32)'A');
	assert(sA.len == 1 && sA[0] == 'A');

	// 0x80 — first 2-byte codepoint (C2 80).
	String s80 = string::tformat("%c", (Char32)0x80);
	assert(s80.len == 2 && s80[0] == 0xC2 && s80[1] == 0x80);

	// 0x800 — first 3-byte codepoint (E0 A0 80).
	String s800 = string::tformat("%c", (Char32)0x800);
	assert(s800.len == 3 && s800[0] == 0xE0 && s800[1] == 0xA0 && s800[2] == 0x80);

	// 0x10000 — first 4-byte codepoint (F0 90 80 80).
	String s10000 = string::tformat("%c", (Char32)0x10000);
	assert(s10000.len == 4 && s10000[0] == 0xF0 && s10000[1] == 0x90
		&& s10000[2] == 0x80 && s10000[3] == 0x80);
}