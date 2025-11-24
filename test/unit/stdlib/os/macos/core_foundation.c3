module std::os::macos::cf @test @if(env::DARWIN);

fn void test_cgfloat_type()
{
	if (env::ARCH_64_BIT)
	{
		assert(CGFloat.sizeof == double.sizeof);
	}
	else
	{
		assert(CGFloat.sizeof == float.sizeof);
	}
}

fn void test_cgpoint()
{
	CGPoint point = { .x = 10.0, .y = 20.0 };

	assert(point.x == 10.0);
	assert(point.y == 20.0);

	CGPoint point2 = { .x = point.x + 5.0, .y = point.y - 10.0 };
	assert(point2.x == 15.0);
	assert(point2.y == 10.0);
}

fn void test_cgsize()
{
	CGSize size = { .width = 100.0, .height = 200.0 };

	assert(size.width == 100.0);
	assert(size.height == 200.0);

	assert(@typeid(size.width) == CGFloat.typeid);
	assert(@typeid(size.height) == CGFloat.typeid);

	CGSize negSize = { .width = -50.0, .height = -75.0 };
	assert(negSize.width == -50.0);
	assert(negSize.height == -75.0);
}

fn void test_cgrect()
{
	CGPoint origin = { .x = 10.0, .y = 20.0 };
	CGSize size = { .width = 100.0, .height = 200.0 };
	CGRect rect = { .origin = origin, .size = size };

	assert(rect.origin.x == 10.0);
	assert(rect.origin.y == 20.0);
	assert(rect.size.width == 100.0);
	assert(rect.size.height == 200.0);

	CGRect rect2 = {
		.origin = { .x = 0.0, .y = 0.0 },
		.size = { .width = 50.0, .height = 75.0 }
	};
	assert(rect2.origin.x == 0.0);
	assert(rect2.size.width == 50.0);
}


fn void test_geometry_struct_sizes()
{
	if (env::ARCH_64_BIT)
	{
		assert(CGPoint.sizeof == 16);  // 2 * double (8 bytes each)
		assert(CGSize.sizeof == 16);   // 2 * double
		assert(CGRect.sizeof == 32);   // CGPoint + CGSize
	}
	else
	{
		assert(CGPoint.sizeof == 8);   // 2 * float (4 bytes each)
		assert(CGSize.sizeof == 8);    // 2 * float
		assert(CGRect.sizeof == 16);   // CGPoint + CGSize
	}
}
