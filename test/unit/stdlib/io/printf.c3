module std::io @test;

fn void printf_int()
{
	String s;
	s = string::new_format("[%-10d]", 78);
	assert(s == "[78        ]");
	s = string::new_format("[%10d]", 78);
	assert(s == "[        78]");
	s = string::new_format("[%010d]", 78);
	assert(s == "[0000000078]");
	s = string::new_format("[%+10d]", 78);
	assert(s == "[       +78]");
	s = string::new_format("[%-+10d]", 78);
	assert(s == "[+78       ]");
}

fn void printf_a()
{
	String s;
	s = string::new_format("%08.2a", 234.125);
	assert(s == "0x1.d4p+7", "got '%s'; want '0x1.d4p+7'", s);
	s = string::new_format("%a", 234.125);
	assert(s == "0x1.d44p+7", "got '%s'; want '0x1.d44p+7'", s);
	s = string::new_format("%A", 234.125);
	assert(s == "0X1.D44P+7", "got '%s'; want '0X1.D44P+7'", s);
	s = string::new_format("%20a", 234.125);
	assert(s == "          0x1.d44p+7", "got '%s'; want '          0x1.d44p+7'", s);
	s = string::new_format("%-20a", 234.125);
	assert(s == "0x1.d44p+7          ", "got '%s'; want '0x1.d44p+7          '", s);
	s = string::new_format("%-20s", "hello world");
	assert(s == "hello world         ", "got '%s'; want 'hello world         '", s);
	s = string::new_format("%20s", "hello world");
	assert(s == "         hello world", "got '%s'; want '         hello world'", s);

	String str = "hello!";
	s = string::new_format("%-20s", str);
	assert(s == "hello!              ", "got '%s'; want 'hello!              '", s);
	s = string::new_format("%20s", str);
	assert(s == "              hello!", "got '%s'; want '              hello!'", s);

	int[2] a = { 12, 23 };
	s = string::new_format("%-20s", a);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	s = string::new_format("%20s", a);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);

	s = string::new_format("%-20s", a[..]);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	s = string::new_format("%20s", a[..]);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);

	float[2] f = { 12.0, 23.0 };
	s = string::new_format("%-24s", f);
	assert(s == "[12.000000, 23.000000]  ", "got '%s'; want '[12.000000, 23.000000]  '", s);
	s = string::new_format("%24s", f);
	assert(s == "  [12.000000, 23.000000]", "got '%s'; want '  [12.000000, 23.000000]'", s);

	int[<2>] vec = { 12, 23 };
	s = string::new_format("%-20s", vec);
	assert(s == "[<12, 23>]          ", "got '%s'; want '[<12, 23>]          '", s);
	s = string::new_format("%20s", vec);
	assert(s == "          [<12, 23>]", "got '%s'; want '          [<12, 23>]'", s);

	String ss = "hello world";
	s = string::new_format("%.4s %.5s", ss, ss);
	assert(s == "hell hello", "got '%s'; want 'hell hello'", s);
}

enum PrintfTest : ushort
{
	ENUMA,
	ENUMB,
}

fn void printf_enum()
{
	String s;

	s = string::new_format("%s", PrintfTest.ENUMA);
	assert(s == "ENUMA", "got '%s'; want 'ENUMA'", s);
	s = string::new_format("%s", PrintfTest.ENUMB);
	assert(s == "ENUMB", "got '%s'; want 'ENUMB'", s);

	s = string::new_format("%d", PrintfTest.ENUMA);
	assert(s == "0", "got '%s'; want '0'", s);
	s = string::new_format("%d", PrintfTest.ENUMB);
	assert(s == "1", "got '%s'; want '1'", s);
}