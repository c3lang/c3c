module std::math::vector @test;
import std::math;

const EPSILON_F = 1e-6f;
const EPSILON_D = 1e-12;

fn void test_vec2_init() @test
{
    Vec2f v1 = { 1.0f, 2.0f };
    Vec2 v2 = { 1.0, 2.0 };
    $assert @typeis(v1[0], float);
    $assert @typeis(v2[0], double);
    assert(v1[0] == 1.0f && v1[1] == 2.0f);
    assert(v2[0] == 1.0 && v2[1] == 2.0);
}

fn void test_vec2_arithmetic() @test
{
    Vec2f v1 = { 1.0f, 2.0f };
    Vec2f v2 = { 2.0f, 3.0f };
    Vec2f sum = v1 + v2;
    assert(math::is_approx_rel(sum[0], 3.0f, EPSILON_F));
    assert(math::is_approx_rel(sum[1], 5.0f, EPSILON_F));
    Vec2f diff = v2 - v1;
    assert(math::is_approx_rel(diff[0], 1.0f, EPSILON_F));
    assert(math::is_approx_rel(diff[1], 1.0f, EPSILON_F));
    Vec2f scaled = v1 * 2.0f;
    assert(math::is_approx_rel(scaled[0], 2.0f, EPSILON_F));
    assert(math::is_approx_rel(scaled[1], 4.0f, EPSILON_F));
}

fn void test_vec2_length() @test
{
    Vec2f v = { 3.0f, 4.0f };
    assert(math::is_approx_rel(v.length_sq(), 25.0f, EPSILON_F));
    Vec2f v2 = { 0.0f, 0.0f };
    assert(math::is_approx_rel(v.distance_sq(v2), 25.0f, EPSILON_F));
}

fn void test_vec3_operations() @test
{
    Vec3f v1 = { 1.0f, 0.0f, 0.0f };
    Vec3f v2 = { 0.0f, 1.0f, 0.0f };
    Vec3f cross = v1.cross(v2);
    assert(math::is_approx_rel(cross[0], 0.0f, EPSILON_F));
    assert(math::is_approx_rel(cross[1], 0.0f, EPSILON_F));
    assert(math::is_approx_rel(cross[2], 1.0f, EPSILON_F));
    float dot = v1.dot(v2);
    assert(math::is_approx_rel(dot, 0.0f, EPSILON_F));
}

fn void test_vec3_transform() @test
{
    Vec3f v = { 1.0f, 0.0f, 0.0f };
    Vec3f axis = { 0.0f, 0.0f, 1.0f };
    Vec3f rotated = v.rotate_axis(axis, math::PI_2);
    assert(math::is_approx_rel(rotated[0] * rotated[0] + rotated[1] * rotated[1], 1.0f, EPSILON_F));
    assert(math::is_approx_rel(rotated[2], 0.0f, EPSILON_F));
    Vec3f perp = v.perpendicular();
    assert(math::is_approx_rel(v.dot(perp), 0.0f, EPSILON_F));
    assert(math::is_approx_rel(perp.length_sq(), v.length_sq(), EPSILON_F));
}

fn void test_vec_magnitude() @test
{
    Vec2f v2 = { 3.0f, 4.0f };
    Vec3f v3 = { 2.0f, 2.0f, 1.0f };
    Vec2f clamped2 = v2.clamp_mag(1.0f, 3.0f);
    assert(math::is_approx_rel(clamped2.length(), 3.0f, EPSILON_F));
    Vec3f clamped3 = v3.clamp_mag(2.0f, 4.0f);
    assert(math::is_approx_rel(clamped3.length(), 3.0f, EPSILON_F));
}

fn void test_vec_interpolation() @test
{
    Vec2f start = { 0.0f, 0.0f };
    Vec2f target = { 10.0f, 0.0f };
    float max_dist = 2.0f;
    Vec2f result = start.towards(target, max_dist);
    assert(math::is_approx_rel(result[0], 2.0f, EPSILON_F));
    assert(math::is_approx_rel(result[1], 0.0f, EPSILON_F));
    Vec2f close_start = { 9.0f, 0.0f };
    result = close_start.towards(target, max_dist);
    assert(math::is_approx_rel(result[0], target[0], EPSILON_F));
    assert(math::is_approx_rel(result[1], target[1], EPSILON_F));
}

fn void test_vec3_advanced() @test
{
    Vec3f v = { 1.0f, 1.0f, 1.0f };
    Vec3f n = { 0.0f, 1.0f, 0.0f };
    float r = 1.5f;
    Vec3f refracted = v.refract(n, r);
    assert(refracted.length_sq() > 0.0f);
    Vec3f a = { 0.0f, 0.0f, 0.0f };
    Vec3f b = { 1.0f, 0.0f, 0.0f };
    Vec3f c = { 0.0f, 1.0f, 0.0f };
    Vec3f p = { 0.25f, 0.25f, 0.0f };
    Vec3f bary = p.barycenter(a, b, c);
    assert(math::is_approx_rel(bary[0] + bary[1] + bary[2], 1.0f, EPSILON_F));
}

fn void test_edge_cases() @test
{
    Vec2f zero2 = { 0.0f, 0.0f };
    Vec3f zero3 = { 0.0f, 0.0f, 0.0f };
    assert(zero2.length_sq() == 0.0f);
    assert(zero3.length_sq() == 0.0f);
    Vec3f perp = zero3.perpendicular();
    assert(perp.length_sq() <= 1.0f + EPSILON_F);
    Vec2f clamped = zero2.clamp_mag(1.0f, 2.0f);
    assert(clamped.length_sq() == 0.0f);
}

fn void test_type_consistency() @test
{
    Vec2f vf = { 1.0f, 2.0f };
    Vec2 vd = { 1.0, 2.0 };
    $assert @typeis(vf.length_sq(), float);
    $assert @typeis(vd.length_sq(), double);
    Vec3f v3f = { 1.0f, 2.0f, 3.0f };
    Vec3 v3d = { 1.0, 2.0, 3.0 };
    $assert @typeis(v3f.cross(v3f)[0], float);
    $assert @typeis(v3d.cross(v3d)[0], double);
}