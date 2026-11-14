module anon_bitstruct_field_collision;

struct InnerStruct
{
	int dummy;
	bool initialized;
}

struct OuterStruct
{
	InnerStruct inner;
	int other;
	bitstruct : char
	{
		bool initialized;
		bool flag;
	}
}

fn void test_anon_bitstruct_name_collision() @test
{
	OuterStruct s = {};
	s.inner.initialized = false;
	s.initialized = true;

	assert(s.initialized == true);
	assert(s.inner.initialized == false);

	s.inner.initialized = true;
	s.initialized = false;

	assert(s.initialized == false);
	assert(s.inner.initialized == true);

	s.flag = true;
	assert(s.flag == true);
	assert(s.initialized == false);
}