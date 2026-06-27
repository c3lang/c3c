module std::math::rectangle @test;
import std::math;
fn void test_intersects()
{
	Rect a = { 0, 0, 10, 10 };
	Rect b = { 5, 5, 10, 10 };
	Rect c = { 10, 10, 10, 10 };

	assert(a.intersects(b));
	assert(!a.intersects(c));
}

fn void test_merge()
{
	Rect a = { 0, 0, 10, 10 };
	Rect b = { 5, 5, 10, 10 };

	Rect r = a.merge(b);
	assert(r.min() == {0,0});
	assert(r.max() == {15,15});
	r = a.merge(a);
	assert(r.min() == {0,0});
	assert(r.max() == {10,10});
}

fn void test_intersection()
{
	Rect a = { 0, 0, 10, 10 };
	Rect b = { 5, 5, 10, 10 };

	Rect r = a.intersection(b)!!;
	assert(r.min() == {5,5});
	assert(r.max() == {10,10});
}

fn void test_inset()
{
	Rect a = { 0, 0, 10, 10 };
	Rect i = a.inset(2);

	test::eq(i.origin, (Vec2){2,2});
	test::eq(i.size, (Vec2){6,6});
}

fn void test_inset_clamped()
{
	Rect a = { 0, 0, 2, 2 };
	Rect i = a.inset_clamped(3);

	test::eq(i.origin, (Vec2){1,1});
	assert(i.size == {0,0});
	a = { 0, 0, 1, 2 };
	i = a.inset_clamped(3);
	test::eq(i.origin, (Vec2){0.5,1});
	assert(i.size == {0,0});
}

fn void test_integral_outer_and_inner()
{
	Rect r1 = {0.2, 0.3, 2.7, 3.8};
	Rect outer1 = r1.integral_outer();
	Rect inner1 = r1.integral_inner();

	assert(outer1.v == {0, 0, 3, 5}, "Was %s", outer1.v);
	assert(inner1.v == {1, 1, 1, 3});

	Rect r2 = {-1.7, -2.2, 3.4, 4.1};
	Rect outer2 = r2.integral_outer();
	Rect inner2 = r2.integral_inner();

	assert(outer2.v == {-2, -3, 4, 5});
	assert(inner2.v == {-1, -2, 2, 3});

	Rect r3 = {0.1, 0.1, 0.4, 0.4};
	Rect outer3 = r3.integral_outer();
	Rect inner3 = r3.integral_inner();

	assert(outer3.v == {0, 0, 1, 1});
	assert(inner3.v == {1, 1, 0, 0});

	Rect r4 = {2, 3, 4, 5};
	Rect outer4 = r4.integral_outer();
	Rect inner4 = r4.integral_inner();

	assert(outer4.v == r4.v);
	assert(inner4.v == r4.v);

	Rect r5 = {1.5, 2.5, 0, 0};
	Rect outer5 = r5.integral_outer();
	Rect inner5 = r5.integral_inner();

	assert(outer5.v == {1, 2, 1, 1});
	assert(inner5.v == {2, 3, 0, 0});
}

fn void test_expand()
{
	Rect a = { 0, 0, 2, 2 };
	Rect e = a.expand(4);

	assert(e.origin == {-4,-4});
	assert(e.size == {10,10});
}

fn void test_merge_point()
{
	Rect r = { 0, 0, 10, 10 };
	Rect e = r.merge_point({20,5});
	assert(e.max() == {20,10});
	e = r.merge_point({5, 5});
	assert(e.max() == {10,10});
}

fn void test_merge_point2()
{
	// This could violate constraints if not correctly implemented.
	Rect r = { .origin = {-6.5519170761108398437500000, -3.9174928665161132812500000 },
				.size = {29.1038341522216796875000000, 17.4623012542724609375000000 }};
	r.merge_point({8, 15});

}

fn void test_clamp_point()
{
	Rect r = { 0, 0, 10, 10 };
	assert(r.clamp_point({-5,5}) == {0,5});
	assert(r.clamp_point({0,15}) == {0,10});
}

alias RectInt = Rectangle {int};
alias Point = int[<2>];

fn void test_int_intersects()
{
	RectInt a = { 0, 0, 10, 10 };
	RectInt b = { 5, 5, 10, 10 };
	RectInt c = { 10, 10, 10, 10 };

	assert(a.intersects(b));
	assert(!a.intersects(c));
}

fn void test_int_merge()
{
	RectInt a = { 0, 0, 10, 10 };
	RectInt b = { 5, 5, 10, 10 };

	RectInt r = a.merge(b);
	assert(r.min() == {0,0});
	assert(r.max() == {15,15});
	r = a.merge(a);
	assert(r.min() == {0,0});
	assert(r.max() == {10,10});
}

fn void test_int_intersection()
{
	RectInt a = { 0, 0, 10, 10 };
	RectInt b = { 5, 5, 10, 10 };

	RectInt r = a.intersection(b)!!;
	assert(r.min() == {5,5});
	assert(r.max() == {10,10});
}

fn void test_int_inset()
{
	RectInt a = { 0, 0, 10, 10 };
	RectInt i = a.inset(2);

	test::eq(i.origin, (Point){2,2});
	test::eq(i.size, (Point){6,6});
}

fn void test_int_inset_clamped()
{
	RectInt a = { 0, 0, 2, 2 };
	RectInt i = a.inset_clamped(3);
	test::eq(i.origin, (Point){1, 1});
	assert(i.size == {0, 0});

	a = { 0, 0, 1, 2 };
	i = a.inset_clamped(3);
	test::eq(i.origin, (Point){0,1});
	assert(i.size == {0,0});
}

fn void test_int_expand()
{
	RectInt a = { 0, 0, 2, 2 };
	RectInt e = a.expand(4);

	assert(e.origin == {-4,-4});
	assert(e.size == {10,10});
}

fn void test_int_merge_point()
{
	RectInt r1 = { 0, 0, 10, 10 };
	RectInt e1 = r1.merge_point({20, 5});
	assert(e1.max() == {20, 10});

	RectInt r2 = { 0, 0, 5, 5 };
	RectInt e2 = r2.merge_point({5, 25});
	assert(e2.max() == {5, 25});

	RectInt r3 = { 0, 0, 15, 9 };
	RectInt e3 = r3.merge_point({10, 18});
	assert(e3.max() == {15, 18});

	RectInt r4 = { 0, 0, 2, 47 };
	RectInt e4 = r4.merge_point({5, 2});
	assert(e4.max() == {5, 47});
}
