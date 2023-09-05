module std::io @test;

fn void printf_a()
{
	String s;
	s = string::printf("%08.2a", 234.125);
	assert(s == "0x1.d4p+7", "got '%s'; want '0x1.d4p+7'", s);
	s = string::printf("%a", 234.125);
	assert(s == "0x1.d44p+7", "got '%s'; want '0x1.d44p+7'", s);
	s = string::printf("%A", 234.125);
	assert(s == "0X1.D44P+7", "got '%s'; want '0X1.D44P+7'", s);
	s = string::printf("%20a", 234.125);
	assert(s == "          0x1.d44p+7", "got '%s'; want '          0x1.d44p+7'", s);
	s = string::printf("%-20a", 234.125);
	assert(s == "0x1.d44p+7          ", "got '%s'; want '0x1.d44p+7          '", s);
	s = string::printf("%-20s", "hello world");
	assert(s == "hello world         ", "got '%s'; want 'hello world         '", s);
	s = string::printf("%20s", "hello world");
	assert(s == "         hello world", "got '%s'; want '         hello world'", s);

	String str = "hello!";
	s = string::printf("%-20s", str);
	assert(s == "hello!              ", "got '%s'; want 'hello!              '", s);
	s = string::printf("%20s", str);
	assert(s == "              hello!", "got '%s'; want '              hello!'", s);

	int[2] a = { 12, 23 };
	s = string::printf("%-20s", a);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	s = string::printf("%20s", a);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);

	s = string::printf("%-20s", a[..]);
	assert(s == "[12, 23]            ", "got '%s'; want '[12, 23]            '", s);
	s = string::printf("%20s", a[..]);
	assert(s == "            [12, 23]", "got '%s'; want '            [12, 23]'", s);

	float[2] f = { 12.0, 23.0 };
	s = string::printf("%-24s", f);
	assert(s == "[12.000000, 23.000000]  ", "got '%s'; want '[12.000000, 23.000000]  '", s);
	s = string::printf("%24s", f);
	assert(s == "  [12.000000, 23.000000]", "got '%s'; want '  [12.000000, 23.000000]'", s);

	int[<2>] vec = { 12, 23 };
	s = string::printf("%-20s", vec);
	assert(s == "[<12, 23>]          ", "got '%s'; want '[<12, 23>]          '", s);
	s = string::printf("%20s", vec);
	assert(s == "          [<12, 23>]", "got '%s'; want '          [<12, 23>]'", s);

	String ss = "hello world";
	s = string::printf("%.4s %.5s", ss, ss);
	assert(s == "hell hello", "got '%s'; want 'hell hello'", s);
}