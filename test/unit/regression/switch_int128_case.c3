module switch_int128_case;

fn void switch_on_wide_case_values() @test
{
	int128 v = (int128)1 << 100;
	switch (v)
	{
		case (int128)1 << 100: break;
		default: unreachable();
	}
	uint128 u = 5;
	switch (u)
	{
		case 5: break;
		case 0x1_0000_0000_0000_0005: unreachable();
		default: unreachable();
	}
}
