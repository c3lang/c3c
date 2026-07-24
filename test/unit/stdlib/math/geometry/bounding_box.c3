module std::math::geometry::bounding_box @test;
import std::math;

fn void test_intersections()
{
	Bounds a = {   0,   0, 10, 10 };
	Bounds b = {   5,   5, 15, 15 };
	Bounds c = {  10,  10, 20, 20 };
	Bounds d = { -10, -10,  0,  0 };

	assert(a.contains_point(a.min));
	assert(a.contains_point(a.max));
	assert(a.contains(a));
	assert(a.intersects(a));
	assert(a.intersects(b));
	assert(a.intersects(c));
	assert(a.intersects(d));

	Bounds r = a.intersection(b)!!;
	assert(r.min == {5,5});
	assert(r.max == {10,10});

	assert(@ok(a.intersection(c)));
	assert(@ok(a.intersection(d)));
}

fn void test_merge()
{
	Bounds a = { 0, 0, 10, 10 };
	Bounds b = { 5, 5, 15, 15 };

	Bounds r = a.merge(b);
	assert(r.min == {0,0});
	assert(r.max == {15,15});
	r = a.merge(a);
	assert(r.min == {0,0});
	assert(r.max == {10,10});
}

fn void test_inset()
{
	Bounds a = { 0, 0, 10, 10 };
	Bounds i = a.inset(2);

	test::eq(i.min, (Vec2){2,2});
	test::eq(i.size(), (Vec2){6,6});
}

fn void test_inset_clamped()
{
	Bounds a = { 0, 0, 2, 2 };
	Bounds i = a.inset_clamped(3);

	test::eq(i.min, (Vec2){1,1});
	assert(i.size() == {0,0});
	a = { 0, 0, 1, 2 };
	i = a.inset_clamped(3);
	test::eq(i.min, (Vec2){0.5,1});
	assert(i.size() == {0,0});
}

fn void test_integral_outer_and_inner()
{
	Bounds b1 = { 0.2, 0.3, 2.9, 4.1 };
	Bounds outer1 = b1.integral_outer();
	Bounds inner1 = b1.integral_inner();

	assert(outer1.v == {0, 0, 3, 5}, "Was %s", outer1.v);
	assert(inner1.v == {1, 1, 2, 4});

	Bounds b2 = { -1.7, -2.2, 1.7, 1.9 };
	Bounds outer2 = b2.integral_outer();
	Bounds inner2 = b2.integral_inner();

	assert(outer2.v == {-2, -3, 2, 2});
	assert(inner2.v == {-1, -2, 1, 1});

	Bounds b3 = { 0.1, 0.1, 0.5, 0.5 };
	Bounds outer3 = b3.integral_outer();
	Bounds inner3 = b3.integral_inner();

	assert(outer3.v == {0, 0 , 1, 1});
	assert(inner3.v == {1, 1 , 1, 1});

	Bounds b4 = { 2, 3, 6, 8 };
	Bounds outer4 = b4.integral_outer();
	Bounds inner4 = b4.integral_inner();

	assert(outer4.v == b4.v);
	assert(inner4.v == b4.v);

	Bounds b5 = { 1.5, 2.5, 1.5, 2.5 };
	Bounds outer5 = b5.integral_outer();
	Bounds inner5 = b5.integral_inner();

	assert(outer5.v == {1, 2, 2, 3});
	assert(inner5.v == {2, 3, 2, 3});
}

fn void test_expand()
{
	Bounds a = { 0, 0, 2, 2 };
	Bounds e = a.expand(4);

	assert(e.min == {-4,-4});
	assert(e.size() == {10,10});
}

fn void test_merge_point()
{
	Bounds b = { 0, 0, 10, 10 };
	Bounds e = b.merge_point({20,5});
	assert(e.max == {20,10});
	e = b.merge_point({5, 5});
	assert(e.max == {10,10});
}

fn void test_offset()
{
	Bounds b = { 0, 0, 10, 10 };
	Bounds e = b + { 10, 5 };
	assert(e.v == {10,5,20,15});
	e = b - { 10, 5 };
	assert(e.v == {-10,-5,0,5});
}

fn void test_clamp_point()
{
	Bounds b = { 0, 0, 10, 10 };
	assert(b.clamp_point({-5,5}) == {0,5});
	assert(b.clamp_point({0,15}) == {0,10});
}
