module std::core::ascii @test;
import std::hash;

fn void test_all()
{
	long x = 0;
	Crc64 check;
	check.init();
	for (char c = 0; c < 255; c++)
	{
		check.updatec(c);
		if (c.is_upper()) check.updatec(1);
		if (c.is_lower()) check.updatec(2);
		if (c.is_alpha()) check.updatec(4);
		if (c.is_bdigit()) check.updatec(8);
		if (c.is_odigit()) check.updatec(16);
		if (c.is_xdigit()) check.updatec(16);
    	if (c.is_digit()) check.updatec(32);
		if (c.is_graph()) check.updatec(64);
		check.updatec(128);
		if (c.is_punct()) check.updatec(1);
		if (c.is_cntrl()) check.updatec(2);
		if (c.is_space()) check.updatec(4);
		check.updatec(c.to_upper());
		check.updatec(c.to_lower());
	}
	test::eq(check.final(), 7327699757963224526UL);
}