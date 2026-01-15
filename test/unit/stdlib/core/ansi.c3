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

fn void test_get_color()
{
	test::eq("\u001B[38;2;255;204;254m", string::tformat("%s", ansi::get_color_rgb(0xFF, 0xCC, 0xFE)));
	test::eq("\u001B[38;2;255;204;254m", string::tformat("%s", ansi::get_color(0xFFCCFE)));
	test::eq("\u001B[48;2;255;204;254m", string::tformat("%s", ansi::get_color(0xFFCCFE, true)));
}
