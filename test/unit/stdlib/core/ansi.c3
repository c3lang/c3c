module std::core::string::ansi @test;

fn void test_color_8bit()
{
	test::eq("\u001B[38;5;255m", ansi::color_8bit(255));
	test::eq("\u001B[48;5;255m", ansi::color_8bit(255, true));
}

fn void test_color_rgb()
{
	test::eq("\u001B[38;2;255;204;255m", ansi::color_rgb(255, 204, 255));
	test::eq("\u001B[48;2;255;204;255m", ansi::color_rgb(255, 204, 255, true));
}

fn void test_color()
{
	test::eq("\u001B[38;2;255;204;255m", ansi::color(0xFFCCFF));
	test::eq("\u001B[48;2;255;204;255m", ansi::color(0xFFCCFF, true));
}

fn void test_make_color()
{
	test::eq("\u001B[38;2;255;204;255m", ansi::make_color(tmem, 0xFFCCFF));
	test::eq("\u001B[48;2;255;204;255m", ansi::make_color(tmem, 0xFFCCFF, true));
}
