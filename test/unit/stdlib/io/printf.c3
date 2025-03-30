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