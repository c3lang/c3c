module std::math::vector @test;
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
    assert(math::is_approx_rel(v.sq_magnitude(), 25.0f, EPSILON_F));
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
    float[<3>] rotated = v.rotate_axis(axis, math::PI_2);
    assert(math::is_approx_rel(rotated[0] * rotated[0] + rotated[1] * rotated[1], 1.0f, EPSILON_F));
    assert(math::is_approx_rel(rotated[2], 0.0f, EPSILON_F));
    float[<3>] perp = v.perpendicular();
    assert(math::is_approx_rel(v.dot(perp), 0.0f, EPSILON_F));
    assert(math::is_approx_rel(perp.sq_magnitude(), v.sq_magnitude(), EPSILON_F));
}

fn void test_vec_magnitude() @test
{
    float[<2>] v2 = { 3.0f, 4.0f };
    float[<3>] v3 = { 2.0f, 2.0f, 1.0f };
    float[<2>] clamped2 = v2.clamp_mag(1.0f, 3.0f);
    assert(math::is_approx_rel(clamped2.length(), 3.0f, EPSILON_F));
    float[<3>] clamped3 = v3.clamp_mag(2.0f, 4.0f);
    assert(math::is_approx_rel(clamped3.length(), 3.0f, EPSILON_F));
}

fn void test_vec_interpolation() @test
{
    float[<2>] start = { 0.0f, 0.0f };
    float[<2>] target = { 10.0f, 0.0f };
    float max_dist = 2.0f;
    float[<2>] result = start.towards(target, max_dist);
    assert(math::is_approx_rel(result[0], 2.0f, EPSILON_F));
    assert(math::is_approx_rel(result[1], 0.0f, EPSILON_F));
    float[<2>] close_start = { 9.0f, 0.0f };
    result = close_start.towards(target, max_dist);
    assert(math::is_approx_rel(result[0], target[0], EPSILON_F));
    assert(math::is_approx_rel(result[1], target[1], EPSILON_F));
}

fn void test_vec3_advanced() @test
{
    float[<3>] v = { 1.0f, 1.0f, 1.0f };
    float[<3>] n = { 0.0f, 1.0f, 0.0f };
    float r = 1.5f;
    float[<3>] refracted = v.refract(n, r);
    assert(refracted.sq_magnitude() > 0.0f);
    float[<3>] a = { 0.0f, 0.0f, 0.0f };
    float[<3>] b = { 1.0f, 0.0f, 0.0f };
    float[<3>] c = { 0.0f, 1.0f, 0.0f };
    float[<3>] p = { 0.25f, 0.25f, 0.0f };
    float[<3>] bary = p.barycenter(a, b, c);
    assert(math::is_approx_rel(bary[0] + bary[1] + bary[2], 1.0f, EPSILON_F));
}

fn void test_edge_cases() @test
{
    float[<2>] zero2 = { 0.0f, 0.0f };
    float[<3>] zero3 = { 0.0f, 0.0f, 0.0f };
    assert(zero2.sq_magnitude() == 0.0f);
    assert(zero3.sq_magnitude() == 0.0f);
    float[<3>] perp = zero3.perpendicular();
    assert(perp.sq_magnitude() <= 1.0f + EPSILON_F);
    float[<2>] clamped = zero2.clamp_mag(1.0f, 2.0f);
    assert(clamped.sq_magnitude() == 0.0f);
}

fn void test_type_consistency() @test
{
    float[<2>] vf = { 1.0f, 2.0f };
    double[<2>] vd = { 1.0, 2.0 };
    $assert $typeof(vf.sq_magnitude()) == float;
    $assert $typeof(vd.sq_magnitude()) == double;
    float[<3>] v3f = { 1.0f, 2.0f, 3.0f };
    double[<3>] v3d = { 1.0, 2.0, 3.0 };
    $assert $typeof(v3f.cross(v3f)[0]) == float;
    $assert $typeof(v3d.cross(v3d)[0]) == double;
}