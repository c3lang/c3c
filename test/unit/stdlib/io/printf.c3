module std::io @test;

fn void printf_a()
{
	assert(string::printf("%08.2a", 234.125) == "0x1.d4p+7");
	assert(string::printf("%a", 234.125) == "0x1.d44p+7");
	assert(string::printf("%A", 234.125) == "0X1.D44P+7");
	assert(string::printf("%20a", 234.125) == "          0x1.d44p+7");
	assert(string::printf("%-20a", 234.125) == "0x1.d44p+7          ");
}

