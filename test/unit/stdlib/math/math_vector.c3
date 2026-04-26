module std::math::vector;
import std::math;

const EPSILON_F = 1e-6f;
const EPSILON_D = 1e-12;

fn void test_vec2_init() @test
{
	float[<2>] v1 = { 1.0f, 2.0f };
	double[<2>] v2 = { 1.0, 2.0 };
	$assert $typeof(v1[0]) == float;
	$assert $typeof(v2[0]) == double;
	assert(v1[0] == 1.0f && v1[1] == 2.0f);
	assert(v2[0] == 1.0 && v2[1] == 2.0);
}

fn void test_vec2_arithmetic() @test
{
	float[<2>] v1 = { 1.0f, 2.0f };
	float[<2>] v2 = { 2.0f, 3.0f };
	float[<2>] sum = v1 + v2;
	assert(math::is_approx_rel(sum[0], 3.0f, EPSILON_F));
	assert(math::is_approx_rel(sum[1], 5.0f, EPSILON_F));
	float[<2>] diff = v2 - v1;
	assert(math::is_approx_rel(diff[0], 1.0f, EPSILON_F));
	assert(math::is_approx_rel(diff[1], 1.0f, EPSILON_F));
	float[<2>] scaled = v1 * 2.0f;
	assert(math::is_approx_rel(scaled[0], 2.0f, EPSILON_F));
	assert(math::is_approx_rel(scaled[1], 4.0f, EPSILON_F));
}

fn void test_vec2_length() @test
{
	float[<2>] v = { 3.0f, 4.0f };
	assert(math::is_approx_rel(v.length_sq(), 25.0f, EPSILON_F));
	float[<2>] v2 = { 0.0f, 0.0f };
	assert(math::is_approx_rel(v.distance_sq(v2), 25.0f, EPSILON_F));
}

fn void test_vec3_operations() @test
{
	float[<3>] v1 = { 1.0f, 0.0f, 0.0f };
	float[<3>] v2 = { 0.0f, 1.0f, 0.0f };
	float[<3>] cross = v1.cross(v2);
	assert(math::is_approx_rel(cross[0], 0.0f, EPSILON_F));
	assert(math::is_approx_rel(cross[1], 0.0f, EPSILON_F));
	assert(math::is_approx_rel(cross[2], 1.0f, EPSILON_F));
	float dot = v1.dot(v2);
	assert(math::is_approx_rel(dot, 0.0f, EPSILON_F));
}

fn void test_vec3_transform() @test
{
	float[<3>] v = { 1.0f, 0.0f, 0.0f };
	float[<3>] axis = { 0.0f, 0.0f, 1.0f };
	float[<3>] rotated = v.rotate_axis(axis, math::HALF_PI);
	assert(math::is_approx_rel(rotated[0] * rotated[0] + rotated[1] * rotated[1], 1.0f, EPSILON_F));
	assert(math::is_approx_rel(rotated[2], 0.0f, EPSILON_F));
	float[<3>] perp = v.perpendicular();
	assert(math::is_approx_rel(v.dot(perp), 0.0f, EPSILON_F));
	assert(math::is_approx_rel(perp.length_sq(), v.length_sq(), EPSILON_F));
}

fn void test_cubic_hermite_start() @test
{
	double[<3>] v1 = {1,2,3};
	double[<3>] v2 = {4,5,6};
	double[<3>] t1 = {1,0,0};
	double[<3>] t2 = {0,1,0};

	double[<3>] r = cubic_hermite(v1, t1, v2, t2, 0.0);

	assert(r.equals(v1));

	v1 = {1,2,3};
	v2 = {4,5,6};
	t1 = {1,0,0};
	t2 = {0,1,0};

	r = cubic_hermite(v1, t1, v2, t2, 1.0);

	assert(r.equals(v2));

	v1 = {0,0,0};
	v2 = {10,0,0};
	double[<3>] t = {10,0,0};

	r = cubic_hermite(v1, t, v2, t, 0.5);

	assert(math::abs(r.x - 5.0) < 1e-9);
	assert(math::abs(r.y) < 1e-9);
	assert(math::abs(r.z) < 1e-9);

	v1 = {0,0,0};
	v2 = {10,0,0};

	r = cubic_hermite(v1, (double[<3>])0, v2, (double[<3>])0, 0.5);

	assert(math::abs(r.x - 5.0) < 1e-9);

	v1 = {0,0,0};
	v2 = {10,0,0};

	t1 = {0,10,0};
	t2 = {0,-10,0};

	r = cubic_hermite(v1, t1, v2, t2, 0.5);

	// Should stay between endpoints in x
	assert(r.x > 0 && r.x < 10);

	// Should curve upward in y
	assert(r.y > 0);

	v1 = {0,0,0};
	v2 = {10,0,0};
	t1 = {0,5,0};
	t2 = {0,-5,0};

	double[<3>] a = cubic_hermite(v1, t1, v2, t2, 0.3);
	double[<3>] b = cubic_hermite(v2,{-t2[0], -t2[1], -t2[2]},
	                              v1,{-t1[0], -t1[1], -t1[2]}, 0.7);

	assert((a - b).length() < 1e-9);
}

fn void test_various_project_unproject(Matrix4 view, Matrix4 proj, double[<4>] viewport, double[<3>] test)
{
	double[<3>] win = test.project(view, proj, viewport);
	double[<3>] res = win.unproject(view, proj, viewport);
	assert(res.equals(test));

}
fn void test_unproject_roundtrip() @test @if(!$feature(OLD_MATRIX))
{
	double[<4>] viewport = {2,3,811,800};
	Matrix4 proj = matrix::perspective(math::PI / 2, 1, 1, 10);
	Matrix4 view = matrix::look_at({0,0,5}, {0,0,0}, {0,1,0});

	test_various_project_unproject(view, proj, viewport, {0,1,0});
	test_various_project_unproject(view, proj, viewport, {0,0,-0.5});
	test_various_project_unproject(view, proj, viewport, {0,1,-10.0});
	test_various_project_unproject(view, proj, viewport, {-1,-1,0.5});
	test_various_project_unproject(view, proj, viewport, {1,1,-0.5});
	test_various_project_unproject(view, proj, viewport, {-0.5,0.5,-0.5});
}

fn void test_vec_magnitude() @test
{
	float[<2>] v2 = { 3.0f, 4.0f };
	float[<3>] v3 = { 2.0f, 2.0f, 1.0f };
	float[<2>] clamped2 = v2.clamp_length(1.0f, 3.0f);
	assert(math::is_approx_rel(clamped2.length(), 3.0f, EPSILON_F));
	float[<3>] clamped3 = v3.clamp_length(2.0f, 4.0f);
	assert(math::is_approx_rel(clamped3.length(), 3.0f, EPSILON_F));
}

fn void test_vec_interpolation() @test
{
	float[<2>] start = { 0.0f, 0.0f };
	float[<2>] target = { 10.0f, 0.0f };
	float max_dist = 2.0f;
	float[<2>] result = start.move_towards(target, max_dist);
	assert(math::is_approx_rel(result[0], 2.0f, EPSILON_F));
	assert(math::is_approx_rel(result[1], 0.0f, EPSILON_F));
	float[<2>] close_start = { 9.0f, 0.0f };
	result = close_start.move_towards(target, max_dist);
	assert(math::is_approx_rel(result[0], target[0], EPSILON_F));
	assert(math::is_approx_rel(result[1], target[1], EPSILON_F));
}

fn void test_vec3_advanced() @test
{
	float[<3>] v = { 1.0f, 1.0f, 1.0f };
	float[<3>] n = { 0.0f, 1.0f, 0.0f };
	float r = 1.5f;
	float[<3>] refracted = v.refract(n, r);
	assert(refracted.length_sq() > 0.0f);
	float[<3>] a = { 0.0f, 0.0f, 0.0f };
	float[<3>] b = { 1.0f, 0.0f, 0.0f };
	float[<3>] c = { 0.0f, 1.0f, 0.0f };
	float[<3>] p = { 0.25f, 0.25f, 0.0f };
	float[<3>] bary = p.barycentric(a, b, c);
	assert(math::is_approx_rel(bary[0] + bary[1] + bary[2], 1.0f, EPSILON_F));
}

fn void test_edge_cases() @test
{
	float[<2>] zero2 = { 0.0f, 0.0f };
	float[<3>] zero3 = { 0.0f, 0.0f, 0.0f };
	assert(zero2.length_sq() == 0.0f);
	assert(zero3.length_sq() == 0.0f);
	float[<3>] perp = zero3.perpendicular();
	assert(perp.length_sq() <= 1.0f + EPSILON_F);
	float[<2>] clamped = zero2.clamp_length(1.0f, 2.0f);
	assert(clamped.length_sq() == 0.0f);
}

fn void test_type_consistency() @test
{
	float[<2>] vf = { 1.0f, 2.0f };
	double[<2>] vd = { 1.0, 2.0 };
	$assert $typeof(vf.length_sq()) == float;
	$assert $typeof(vd.length_sq()) == double;
	float[<3>] v3f = { 1.0f, 2.0f, 3.0f };
	double[<3>] v3d = { 1.0, 2.0, 3.0 };
	$assert $typeof(v3f.cross(v3f)[0]) == float;
	$assert $typeof(v3d.cross(v3d)[0]) == double;
}

fn void test_rejection() @test
{
	double[<3>] a = {0, 1, 0};
	double[<3>] b = {1, 0, 0};
	double[<3>] rejection = a.rejection(b);
	assert(rejection.equals({0, 1, 0}), "Was %s", rejection);

	a = {1,0,0};
	b = {2,0,0};
	rejection = a.rejection(b);
	assert(rejection.equals({0,0,0}));

	a = {1,0,0};
	b = {-3,0,0};
	rejection = a.rejection(b);
	assert(rejection.equals({0,0,0}));

	a = {1,2,3};
	b = {4,5,6};
	rejection = a.rejection(b);
	assert(rejection.equals({-0.66233766, -0.07792208, 0.50649351}), "Was %s", rejection);

	a = {1,0,0};
	b = {1,1e-8,0};
	rejection = a.rejection(b);
	assert(rejection.equals({0,1e-8,0}), "Was %s", rejection);

	a = {0,1,1e-8};
	b = {1,0,0};
	rejection = a.rejection(b);
	assert(rejection.equals({0,1,1e-8}), "Was %s", rejection);
}

fn void test_angle() @test
{
	double[<3>] a = {1, 0, 0};
	double[<3>] b = {0, 1, 0};
	double angle = a.angle(b);
	assert(math::is_approx(angle, math::PI/2, 1e-6)); // 90° = π/2

	a = {1, 0, 0};
	b = {2, 0, 0};
	angle = a.angle(b);
	assert(math::is_approx(angle, 0, 1e-6)); // 0°

	a = {1, 0, 0};
	b = {-1, 0, 0};
	angle = a.angle(b);
	assert(math::is_approx(angle, math::PI, 1e-6)); // 180° = π

	a = {0, 0, 0};
    b = {1, 0, 0};
	angle = a.angle(b);
	assert(angle == 0.0); // Should safely return 0

	a = {1, 2, 3};
    b = {4, 5, 6};
	angle = a.angle(b);
    // Compare to precomputed value ≈ 0.2257 radians
    assert(math::is_approx(angle, 0.225726, 1e-6));

	a = {1, 0, 0};
	b = {1, 1e-8, 0};  // tiny deviation
	angle = a.angle(b);
    assert(math::is_approx(angle, 1e-8, 1e-10));

	a = {1, 0, 0};
	b = {0, 1 - 1e-8, 0};  // slightly less than 90°
	angle = a.angle(b);
    assert(math::is_approx(angle, math::PI/2, 1e-8));

	a = {1, 0, 0};
	b = {1, 1e-9, 1e-9};  // almost parallel in 3D
	angle = a.angle(b);
    assert(math::is_approx(angle, 1.4142136e-9, 1e-12));
}
