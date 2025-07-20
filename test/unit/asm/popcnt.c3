module test::inline_asm::popcnt @if (env::X86_64);

fn void popcntw() @test
{
	short src   = 0xF0;
	short count = 0;
	asm { popcntw count, src; }
	assert(count == 4, "inline asm: popcntw failed");
}

fn void popcntl() @test
{
	int src   = 0xF0F0;
	int count = 0;
	asm { popcntl count, src; }
	assert(count == 8, "inline asm: popcntl failed");
}

fn void popcntq() @test
{
	long src   = 0xF0F0_F0F0;
	long count = 0;
	asm { popcntq count, src; }
	assert(count == 16, "inline asm: popcntq failed");
}
