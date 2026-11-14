module jump_switch_small_types;

fn int jump_ichar(ichar x)
{
	switch (x) @jump
	{
		case -100: return 1;
		case 0: return 2;
		case 100: return 3;
		default: return 0;
	}
}

fn int jump_ichar_full(ichar x)
{
	switch (x) @jump
	{
		case -128: return 1;
		case 127: return 2;
		default: return 0;
	}
}

fn int jump_short(short x)
{
	switch (x) @jump
	{
		case -1000: return 1;
		case 1000: return 2;
		default: return 0;
	}
}

fn int jump_char(char x)
{
	switch (x) @jump
	{
		case 0: return 1;
		case 255: return 2;
		default: return 0;
	}
}

fn void jump_switch_on_ichar_wide_span() @test
{
	assert(jump_ichar(-100) == 1);
	assert(jump_ichar(0) == 2);
	assert(jump_ichar(100) == 3);
	assert(jump_ichar(50) == 0);
	assert(jump_ichar(-128) == 0);
	assert(jump_ichar(127) == 0);
	assert(jump_ichar_full(-128) == 1);
	assert(jump_ichar_full(127) == 2);
	assert(jump_ichar_full(0) == 0);
}

fn void jump_switch_on_short_wide_span() @test
{
	assert(jump_short(-1000) == 1);
	assert(jump_short(1000) == 2);
	assert(jump_short(0) == 0);
	assert(jump_short(short::min) == 0);
	assert(jump_short(short::max) == 0);
}

fn void jump_switch_on_char_full_span() @test
{
	assert(jump_char(0) == 1);
	assert(jump_char(255) == 2);
	assert(jump_char(128) == 0);
}
