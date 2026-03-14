module math_matrix @test;
import std::math;

fn void test_mat4_translate()
{
	Matrix4 mat = MATRIX4_IDENTITY;
	Matrix4 mat2 = { .m00 = 1, .m11 = 1, .m22 = 1, .m33 = 1 };
	Matrix4 calc = mat.mul(mat2);
	assert(calc == mat);
	assert(mat * mat2 == mat);

	Matrix4 translated = mat.translate({0.0, 0.0, 0.0});
	assert(translated == mat);
}

fn void test_mat3_translate()
{
	Matrix3 m = MATRIX3_IDENTITY;
	m = m.translate({3, 4});
	assert(m == (Matrix3){ .m00 = 1, .m11 = 1, .m22 = 1, .m02 = 3, .m12 = 4 });
	m = { .m00 = 1, .m11 = 1, .m22 = 1, .m02 = 3, .m12 = 4 };
	assert(m.translate({2, 1}) == (Matrix3){ .m00 = 1, .m11 = 1, .m22 = 1, .m02 = 5, .m12 = 5 });

	m = { .m00 = 2, .m11 = 3, .m22 = 1 };
	assert(m.translate({1, 2}) == (Matrix3){ .m00 = 2, .m11 = 3, .m22 = 1, .m02 = 2, .m12 = 6 });

	m = { .m10 = 1, .m01 = -1, .m22 = 1 };
	assert(m.translate({2, 0}) == (Matrix3){ .m10 = 1, .m01 = -1, .m12 = 2, .m22 = 1 });
}

fn void test_mat3_scale()
{
	Matrix3 m = MATRIX3_IDENTITY;

	m = m.scale({2,3});

	Matrix3 res1 = { .m00 = 2, .m11 = 3, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.m, 1) == (double[<9>])res1.m);

	m = { .m10 = 1, .m01 = -1, .m22 = 1 };
	m = m.scale({2,3});

	Matrix3 res2 = { .m10 = 2, .m01 = -3, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.m, 1) == (double[<9>])res2.m, "Was %s", m.m);

	m = { .m00 = 1, .m11 = 1, .m02 = 5, .m12 = 6, .m22 = 1 };

	Matrix3 res3 = { .m00 = 2, .m11 = 3, .m02 = 5, .m12 = 6, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.scale({2,3}).m, 1) == (double[<9>])res3.m);
}


fn void test_mat3_rotate()
{
	Matrix3 m = MATRIX3_IDENTITY;
	m = m.rotate(math::PI / 2);
	Matrix3 res = { .m10 = 1, .m01 = -1, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.m, 1) == (double[<9>])res.m, "Was %s", m.m);

	m = MATRIX3_IDENTITY;
	m = m.rotate(math::PI / 2);
	m = m.rotate(math::PI / 2);

	res = { .m00 = -1, .m11 = -1, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.m, 1) == (double[<9>])res.m);

	m = { .m00 = 1, .m11 = 1, .m02 = 3, .m12 = 4, .m22 = 1 };

	Matrix3 r = m.rotate(math::PI / 2);

	res = { .m10 = 1, .m01 = -1, .m02 = 3, .m12 = 4, .m22 = 1 };

	assert(math::round_to_decimals((double[<9>])r.m, 1) == (double[<9>])res.m);
}

fn void test_mat4_translate_v()
{
	Matrix4 mat = MATRIX4_IDENTITY.translate({1.0, 2.0, 3.0});
	double[<4>] res = math::round_to_decimals(mat * { 10.0, 11.0, 12.0, 1.0}, 4);
	assert(res == {11.0, 13.0, 15.0, 1.0}, "Was %s", res);
}


fn void test_mat4_rotate()
{
	Matrix4 rot_x = MATRIX4_IDENTITY.rotate_x(math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_x * (double[<4>]){ 0.0, 1.0, 0.0, 1.0}, 4) == {0.0, 0.0, 1.0, 1.0});
	Matrix4f rot_xf = MATRIX4F_IDENTITY.rotate_x((float) math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_xf * (float[<4>]){ 0.0, 1.0, 0.0, 1.0}, 4) == {0.0, 0.0, 1.0, 1.0});

	Matrix4 rot_y = MATRIX4_IDENTITY.rotate_y(math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_y * (double[<4>]){ 1.0, 0.0, 0.0, 1.0}, 4) == {0.0, 0.0, -1.0, 1.0});
	Matrix4f rot_yf = MATRIX4F_IDENTITY.rotate_y((float) math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_yf * (float[<4>]){ 1.0, 0.0, 0.0, 1.0}, 4) == {0.0, 0.0, -1.0, 1.0});

	Matrix4 rot_z = MATRIX4_IDENTITY.rotate_z(math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_z * (double[<4>]){ 0.0, 1.0, 0.0, 1.0}, 4) == {-1.0, 0.0, 0.0, 1.0});
	Matrix4f rot_zf = MATRIX4F_IDENTITY.rotate_z((float) math::deg_to_rad(90));
	assert(math::round_to_decimals(rot_zf * (float[<4>]){ 0.0, 1.0, 0.0, 1.0}, 4) == {-1.0, 0.0, 0.0, 1.0});
}

fn void test_mat4_scale()
{
	Matrix4 scale = MATRIX4_IDENTITY.scale({2.0, 3.0, 4.0});
	assert(math::round_to_decimals(scale * (double[<4>]){ 10.0, 11.0, 12.0, 1.0}, 4) == {20.0, 33.0, 48.0, 1.0});
}

fn void test_mat4_mul()
{
	Matrix4 mat = { 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8 };
	Matrix4 mat2 = { 8, 7, 6, 5, 4, 3, 2, 1, 8, 7, 6, 5, 4, 3, 2, 1 };
	Matrix4 calc = mat.mul(mat2);
	Matrix4 value = { 74,100,126,152, 26,36,46,56, 74,100,126,152, 26,36,46,56 };
	assert(math::round_to_decimals((double[<16>])calc.m,3) == (double[<16>])value.m);
	assert(math::round_to_decimals((double[<16>])(mat * mat2).m, 3) == (double[<16>])value.m);
}

fn void test_mat4_lookat()
{
	Matrix4 result = {
		.m00 = 0.988936, .m10 = -0.014599, .m20 = 0.147620,
		.m11 = 0.995146, .m21 = 0.098414,
		.m02 = -0.148340, .m12 = -0.097325, .m22 = 0.984136,
		.m03 = -0.988936, .m13 = -2.970838, .m23 = -20.765262, .m33 = 1.000000
	};
	Matrix4f result_f = {
		.m00 = 0.988936, .m10 = -0.014599, .m20 = 0.147620,
		.m11 = 0.995146, .m21 = 0.098414,
		.m02 = -0.148340, .m12 = -0.097325, .m22 = 0.984136,
		.m03 = -0.988936, .m13 = -2.970838, .m23 = -20.765262, .m33 = 1.000000
	};


	Matrix4 look_at = matrix4_look_at({4.0, 5.0, 20.0}, {1.0, 3.0, 0.0}, {0.0, 1.0, 0.0});
	Matrix4f look_at_f = matrix4f_look_at({4.0, 5.0, 20.0}, {1.0, 3.0, 0.0}, {0.0, 1.0, 0.0});

	assert(math::round_to_decimals((double[<16>])look_at.m, 4) == math::round_to_decimals((double[<16>])result.m, 4), "Was %s", look_at);
	assert(math::round_to_decimals((double[<16>])look_at_f.m, 4) == math::round_to_decimals((double[<16>])result_f.m, 4));
}

fn void test_mat4_perspective() @if(!$feature(OLD_MATRIX))
{
	Matrix4 perspective = matrix4_perspective(math::deg_to_rad(90), 1, 1, 10);
	Matrix4 res = { .m00 = 1, .m11 = 1, .m22 = -1.111111, .m32 = -1, .m23 = -1.111111 };
	assert(perspective.m.equals(res.m));


	perspective = matrix4_perspective(math::deg_to_rad(60), 1, 0.1, 100);
	res = { 1.732051,0,0,0, 0,1.732051,0,0, 0,0,-1.001001,-1, 0,0,-0.100100,0 };
	assert(perspective.m.equals(res.m));

	perspective = matrix4_perspective(math::deg_to_rad(45), 1.5, 0.5, 50);
	res = { 1.609476,0,0,0, 0,2.414214,0,0, 0,0,-1.010101,-1, 0,0,-0.505051,0 };
	assert(math::abs(perspective.m - res.m).max() < 1e-6, "Was %s, expected %s", perspective, res);

	perspective = matrix4_perspective(2.094395, 0.75, 0.01, 1000.0);
	res = { 0.769800,0,0,0, 0,0.577350,0,0, 0,0,-1.000010,-1, 0,0,-0.0100001,0 };
	assert(math::abs(perspective.m - res.m).max() < 1e-6);

}

fn void test_mat4_ortho() @if(!$feature(OLD_MATRIX))
{
	Matrix4 o = matrix4_ortho(-1, 1, -1, 1, 1, 10);
	Matrix4 res = { 1.0,0,0,0, 0,1.0,0,0, 0,0,0.111111,0, -0,0,-1.111111,1 };
	assert(res.m.equals(res.m));

	o = matrix4_ortho(0, 2, 0, 2, 0.1, 100);
	res = { .m00 = 1, .m11 = 1, .m22 = -0.01001, .m03 = -1, .m13 = -1, .m23 = -0.001001, .m33 = 1 };
	assert(math::abs(o.m - res.m).max() < 1e-6, "Was %s, expected %s", o, res);

	o = matrix4_ortho(-2, 2, -1, 1, 0.5, 50);
	res = {  0.5, 0, 0, 0, 0, 1.0, 0, 0, 0, 0, -0.020202, 0, 0, 0, -0.010101, 1 };
	assert(math::abs(o.m - res.m).max() < 1e-6, "Was %s, expected %s", o, res);


	o = matrix4_ortho(-10, 10, -5, 5, 0.01, 1000);
	res = { 0.1, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, -0.00100001, 0, 0, 0, -0.000010, 1 };
	assert(math::abs(o.m - res.m).max() < 1e-6, "Was %s, expected %s", o, res);

	o = matrix4_ortho(-5, 5, -5, 5, 0.1, 10);
	res = { 0.2, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, -0.101010, 0, 0, 0, -0.010101, 1 };
	assert(math::abs(o.m - res.m).max() < 1e-6, "Was %s, expected %s", o, res);

}

fn void test_apply()
{
	Matrix3 mat = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	double[<3>] res = mat.apply({7, 8, 9});
	assert(res == { 50.000000, 122.000000, 194.000000 });
}

fn void test_mat3()
{

	Matrix3 mat = { .m00 = 3, .m10 = 5, .m20 = 6, .m01 = 5, .m11 = 2, .m21 = 2, .m02 = 3, .m12 = 6, .m22 = 1 };
	Matrix3 mat2 = { .m00 = 4, .m10 = 7, .m20 = 2, .m01 = 2, .m11 = 8, .m21 = 3, .m02 = 6, .m12 = 9, .m22 = 4 };
	Matrix3 calc = mat.mul(mat2);
	Matrix3 value = { .m00 = 53, .m01 = 55, .m02 = 75, .m10 = 46, .m11 = 44, .m12 = 72, .m20 = 40, .m21 = 31, .m22 = 58 };

	assert(calc == value);
}

fn void test_mat3_det()
{
	Matrix3 mat = { .m00 = 1, .m20 = 1, .m01 = 2, .m11 = 4, .m02 = 3, .m12 = 5, .m22 = 6 };
	assert(mat.determinant() == 22);
}

fn void test_mat3_inverse()
{
	Matrix3 m = MATRIX3_IDENTITY;
	assert(m.inverse()!! == MATRIX3_IDENTITY);

	m = MATRIX3_IDENTITY.translate({3,4});
	Matrix3 res = { .m00 = 1, .m11 = 1, .m02 = -3, .m12 = -4, .m22 = 1 };
	assert(math::round_to_decimals((double[<9>])m.adjoint().m, 2) == (double[<9>])res.m, "was %s expected %s", m.adjoint().m, res.m);
	assert(math::round_to_decimals((double[<9>])m.inverse()!!.m, 2) == (double[<9>])res.m, "was %s expected %s", m.inverse()!!.m, res.m);

	m = MATRIX3_IDENTITY
		.translate({3,4})
		.rotate(math::PI / 3)
		.scale({2,3});

	Matrix3 inv = m.inverse()!!;

	Matrix3 id = m * inv;

	Matrix3 ident = MATRIX3_IDENTITY;
	assert(math::round_to_decimals((double[<9>])id.m, 2) == (double[<9>])ident.m);
}

fn void test_mat4_z_rotation()
{
	double angle = math::PI * 0.5;

	Matrix4 m = MATRIX4_IDENTITY.rotate_z(angle);

	double[<4>] v = { 1, 0, 0, 1 };
	double[<4>] r = m * v;

	assert(math::abs(r.x) < 1e-6);
	assert(math::abs(r.y - 1) < 1e-6);
	assert(math::abs(r.z) < 1e-6);
}

fn void test_mat4_x_rotation()
{
	double angle = math::PI * 0.5;

	Matrix4 m = MATRIX4_IDENTITY.rotate_x(angle);

	// Y axis -> Z axis
	double[<4>] v1 = { 0, 1, 0, 1 };
	double[<4>] r1 = m * v1;

	assert(math::abs(r1.x) < 1e-6);
	assert(math::abs(r1.y) < 1e-6);
	assert(math::abs(r1.z - 1) < 1e-6);

	// Z axis -> -Y axis
	double[<4>] v2 = { 0, 0, 1, 1 };
	double[<4>] r2 = m * v2;

	assert(math::abs(r2.x) < 1e-6);
	assert(math::abs(r2.y + 1) < 1e-6);
	assert(math::abs(r2.z) < 1e-6);
}

fn void test_mat4_y_rotation()
{
	double angle = math::PI * 0.5;

	Matrix4 m = MATRIX4_IDENTITY.rotate_y(angle);

	double[<4>] v = { 0, 0, 1, 1 };
	double[<4>] r = m * v;

	assert(math::abs(r.x - 1) < 1e-6);
	assert(math::abs(r.y) < 1e-6);
	assert(math::abs(r.z) < 1e-6);
}

fn void test_mat4_inverse()
{
	Matrix4 m = MATRIX4_IDENTITY
		.translate({3,4, 5})
		.rotate_z(math::PI / 3)
		.scale({2,3, 6});

	Matrix4 inv = m.inverse()!!;

	Matrix4 id = m * inv;

	Matrix4 ident = MATRIX4_IDENTITY;
	assert(math::round_to_decimals((double[<16>])id.m, 2) == (double[<16>])ident.m);
}


fn void test_mat2()
{
	Matrix2 mat = { 3, 5, 5, 2};
	Matrix2 mat2 = { 4, 7, 2, 8 };
	Matrix2 calc = mat.mul(mat2);
	Matrix2 value = { 47, 34, 46, 26 };

}

fn void test_mat2_inverse()
{
	Matrix2 a = { 3, 6, 5, 2 };
	Matrix2 a_inv = a.inverse()!!;
	double sum = ((double[<4>])a_inv.mul(a).m).sum();
	assert(math::abs(sum - 2.0) < math::FLOAT_EPSILON,
		"wrong inverse: sum of all elements should be 2, but got: %g", sum);
}

fn void test_vec3()
{
	double[<3>] cross = (double[<3>]){2,3,4}.cross({5,6,7});
	assert(cross == {-3,6,-3});
}
