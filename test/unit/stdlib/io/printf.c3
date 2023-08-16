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
}