module test::inline_asm::combined @feat(X86_64);

fn void test_asm_combined() @test
{
	int x = 0x0F;
	int y = 0xF0;
	int res = 0;

	asm("nop");

	asm {
		movl res, x;
		addl res, y;
		popcnt res, res;
	}

	asm("nop\n\tnop");

	assert(res == 8, "combined asm failed");
}

fn void test_asm_mem_and_imm() @test
{
	int val = 35;
	int res = 0;

	asm("nop");

	asm {
		movl res, [&val];
		addl res, 5;
	}

	assert(res == 40, "mem and imm asm failed");
}