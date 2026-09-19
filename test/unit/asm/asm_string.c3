module test::inline_asm::asm_string;

fn void test_asm_string() @test
{
	int x = 1;
	asm("nop");
	assert(x == 1);
}