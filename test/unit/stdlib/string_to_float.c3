module string_to_float_tests;

fn void test_float() @test
{
	assert(String.to_float("1.2")!! == 1.2f);
	assert(String.to_float("10")!! == 10f);
	assert(String.to_float(".7647834")!! == 0.7647834f);
	assert(String.to_float("0.213232")!! == 0.213232f);
	assert(String.to_float("000001.487348")!! == 000001.487348f);
	assert(String.to_float("3.54500000")!! == 3.54500000f);
	assert(String.to_float("4.0")!! == 4.0f);
	assert(String.to_float("-23.545")!! == -23.545f);
	assert(String.to_float("1.5555555555555")!! == 1.5555555555555f);
	assert(String.to_float("1.5555555555556666")!! == 1.5555555555556666f);
}

fn void test_double() @test
{
	assert(String.to_double("1.2")!! == 1.2);
	assert(String.to_double("10")!! == 10);
	assert(String.to_double(".7647834")!! == 0.7647834);
	assert(String.to_double("0.213232")!! == 0.213232);
	assert(String.to_double("000001.487348")!! == 000001.487348);
	assert(String.to_double("3.54500000")!! == 3.54500000);
	assert(String.to_double("4.0")!! == 4.0);
	assert(String.to_double("-23.545")!! == -23.545);
	assert(String.to_double("1.5555555555555")!! == 1.5555555555555);
	assert(String.to_double("1.5555555555556666")!! == 1.5555555555556666);
}