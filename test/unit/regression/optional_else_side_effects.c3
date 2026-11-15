module optional_else_side_effects;

faultdef OESE_BAD;

int oese_counter;

fn fault oese_get() { oese_counter++; return OESE_BAD; }
fn int oese_value() { oese_counter += 10; return 5; }

fn void fault_optional_else_keeps_side_effects() @test
{
	oese_counter = 0;
	int x = oese_get()~ ?? 5;
	assert(x == 5);
	assert(oese_counter == 1);
	int y = (oese_get()~) ?? 6;
	assert(y == 6);
	assert(oese_counter == 2);
}

fn void fault_optional_else_order() @test
{
	oese_counter = 0;
	int x = oese_get()~ ?? oese_value();
	assert(x == 5);
	assert(oese_counter == 11);
}

fn void constant_fault_optional_else() @test
{
	int x = OESE_BAD~ ?? 7;
	assert(x == 7);
}
