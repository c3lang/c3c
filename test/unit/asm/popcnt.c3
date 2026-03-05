module test::inline_asm::popcnt @if (env::X86_64);

fn void popcntw() @test
{
	short src   = 0xF0;
	short count = 0;
	asm { popcnt count, src; }
	assert(count == 4, "inline asm: popcnt failed");
}

fn void popcntl() @test
{
	int src   = 0xF0F0;
	int count = 0;
	asm { popcnt count, src; }
	assert(count == 8, "inline asm: popcnt failed");
}

fn void popcntq() @test
{
	long src   = 0xF0F0_F0F0;
	long count = 0;
	asm { popcnt count, src; }
	assert(count == 16, "inline asm: popcnt failed");
}
