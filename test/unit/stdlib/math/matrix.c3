module math_matrix @test;
import std::math;

fn void test_mat4_translate()
{
	Matrix4 mat = MATRIX4_IDENTITY;
	Matrix4 mat2 = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 };
	Matrix4 calc = mat.mul(mat2);
	assert(calc.m == mat.m);
	assert(mat * mat2 == mat);

	Matrix4 translated = mat.translate({0.0, 0.0, 0.0});
	assert(translated.m == mat.m);
}

fn void test_mat4_translate_v()
{
	Matrix4 mat = MATRIX4_IDENTITY.translate({1.0, 2.0, 3.0});
	assert(math::round_to_decimals(mat * (double[<4>]){ 10.0, 11.0, 12.0, 1.0}, 4) == {11.0, 13.0, 15.0, 1.0});
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
	Matrix4 value = { 56, 46, 36, 26, 152, 126, 100, 74, 56, 46, 36, 26, 152, 126, 100, 74 };
	assert(calc.m == value.m);
	assert(mat * mat2 == value);
}

fn void test_mat4_lookat()
{
	Matrix4 result = {
		0.988936, 0.000000, -0.148340, -0.988936,
		-0.014599, 0.995146, -0.097325, -2.970838,
		0.147620, 0.098414, 0.984136, -20.765262,
		0.000000, 0.000000, 0.000000, 1.000000
	};

	Matrix4f result_f = {
		0.988936, 0.000000, -0.148340, -0.988936,
		-0.014599, 0.995146, -0.097325, -2.970838,
		0.147620, 0.098414, 0.984136, -20.765262,
		0.000000, 0.000000, 0.000000, 1.000000
	};

	Matrix4 result_transposed = {
		0.988936, -0.014599, 0.147620, 0.000000,
		0.000000, 0.995146, 0.098414, 0.000000,
		-0.148340, -0.097325, 0.984136, 0.000000,
		-0.988936, -2.970838, -20.765262, 1.000000
	};

	Matrix4f result_transposed_f = {
		0.988936, -0.014599, 0.147620, 0.000000,
		0.000000, 0.995146, 0.098414, 0.000000,
		-0.148340, -0.097325, 0.984136, 0.000000,
		-0.988936, -2.970838, -20.765262, 1.000000
	};

	Matrix4 look_at = matrix::look_at{double}({4.0, 5.0, 20.0}, {1.0, 3.0, 0.0}, {0.0, 1.0, 0.0});
	Matrix4f look_at_f = matrix::look_at{float}({4.0, 5.0, 20.0}, {1.0, 3.0, 0.0}, {0.0, 1.0, 0.0});

	assert(math::round_to_decimals((double[<16>])look_at.m, 4) == math::round_to_decimals((double[<16>])result.m, 4));
	assert(math::round_to_decimals((float[<16>])look_at_f.m, 4) == math::round_to_decimals((float[<16>])result_f.m, 4));

	assert(math::round_to_decimals((double[<16>])result_transposed.m, 4) == math::round_to_decimals((double[<16>])look_at.transpose().m, 4));
	assert(math::round_to_decimals((float[<16>])result_transposed_f.m, 4) == math::round_to_decimals((float[<16>])look_at_f.transpose().m, 4));
}

fn void test_mat4_perspective()
{
	Matrix4 result = {
		1.857087, 0.000000, 0.000000,
		0.000000, 0.000000, 2.414214,
		0.000000, 0.000000, 0.000000, 0.000000,
		-1.000200, -0.200020, 0.000000, 0.000000,
		-1.000000, 0.000000
	};

	Matrix4f result_f = {
		1.857087, 0.000000, 0.000000,
		0.000000, 0.000000, 2.414214,
		0.000000, 0.000000, 0.000000, 0.000000,
		-1.000200, -0.200020, 0.000000, 0.000000,
		-1.000000, 0.000000
	};

	Matrix4 perspective = matrix4_perspective(math::deg_to_rad(45), 1.3, 0.1, 1000);
	Matrix4f perspective_f = matrix4f_perspective((float)math::deg_to_rad(45), 1.3, 0.1, 1000);

	assert(math::round_to_decimals((double[<16>])result.m, 4) == math::round_to_decimals((double[<16>])perspective.m, 4));
	assert(math::round_to_decimals((float[<16>])result_f.m, 4) == math::round_to_decimals((float[<16>])perspective_f.m, 4));

	// Test the corners of the view frustum
	double top = math::tan(math::deg_to_rad(45) / 2);
	double right = math::tan(math::deg_to_rad(45) / 2) * 1.3;
	
	assert(math::round_to_decimals(perspective * (double[<4>]) {-right * 0.1, -top * 0.1, -0.1, 1}, 4) == {-0.1, -0.1, -0.1, 0.1});
	assert(math::round_to_decimals(perspective * (double[<4>]) { right * 0.1, -top * 0.1, -0.1, 1}, 4) == { 0.1, -0.1, -0.1, 0.1});
	assert(math::round_to_decimals(perspective * (double[<4>]) {-right * 0.1,  top * 0.1, -0.1, 1}, 4) == {-0.1,  0.1, -0.1, 0.1});
	assert(math::round_to_decimals(perspective * (double[<4>]) { right * 0.1,  top * 0.1, -0.1, 1}, 4) == { 0.1,  0.1, -0.1, 0.1});
	assert(math::round_to_decimals(perspective * (double[<4>]) {-right * 1000, -top * 1000, -1000, 1}, 4) == {-1000, -1000, 1000, 1000});
	assert(math::round_to_decimals(perspective * (double[<4>]) { right * 1000, -top * 1000, -1000, 1}, 4) == { 1000, -1000, 1000, 1000});
	assert(math::round_to_decimals(perspective * (double[<4>]) {-right * 1000,  top * 1000, -1000, 1}, 4) == {-1000,  1000, 1000, 1000});
	assert(math::round_to_decimals(perspective * (double[<4>]) { right * 1000,  top * 1000, -1000, 1}, 4) == { 1000,  1000, 1000, 1000});
}

fn void test_mat4_ortho()
{
	Matrix4 result = {
		0.0025, 0, 0, -0.5,
		0, 0.005, 0, 0,
		0, 0, -0.05, -0.5,
		0, 0, 0, 1
	};
	Matrix4f result_f = {
		0.0025, 0, 0, -0.5,
		0, 0.005, 0, 0,
		0, 0, -0.05, -0.5,
		0, 0, 0, 1
	};

	Matrix4 ortho = matrix4_ortho(-200, 600, 200, -200, -10, 30);
	Matrix4f ortho_f = matrix4f_ortho(-200, 600, 200, -200, -10, 30);
	
	assert(math::round_to_decimals((double[<16>])result.m, 4) == math::round_to_decimals((double[<16>])ortho.m, 4));
	assert(math::round_to_decimals((double[<16>])result_f.m, 4) == math::round_to_decimals((double[<16>])ortho_f.m, 4));

	// Test the corners of the view space
	assert(math::round_to_decimals(ortho * (double[<4>]) {-200, -200,  10, 1}, 4) == {-1.0, -1.0, -1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) { 600, -200,  10, 1}, 4) == { 1.0, -1.0, -1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) {-200,  200,  10, 1}, 4) == {-1.0,  1.0, -1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) { 600,  200,  10, 1}, 4) == { 1.0,  1.0, -1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) {-200, -200, -30, 1}, 4) == {-1.0, -1.0,  1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) { 600, -200, -30, 1}, 4) == { 1.0, -1.0,  1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) {-200,  200, -30, 1}, 4) == {-1.0,  1.0,  1.0, 1.0});
	assert(math::round_to_decimals(ortho * (double[<4>]) { 600,  200, -30, 1}, 4) == { 1.0,  1.0,  1.0, 1.0});
}

fn void test_mat3()
{
		Matrix3 mat = { 3, 5, 3, 5, 2, 6, 6, 2, 1 };
		Matrix3 mat2 = { 4, 2, 6, 7, 8, 9, 2, 3, 4 };
		Matrix3 calc = mat.mul(mat2);
		Matrix3 value = { 53, 55, 75, 46, 44, 72, 40, 31, 58 };

		assert(calc.m == value.m);
}

fn void test_mat2()
{
		Matrix2 mat = { 3, 5, 5, 2};
		Matrix2 mat2 = { 4, 2, 7, 8 };
		Matrix2 calc = mat.mul(mat2);
		Matrix2 value = { 47, 46, 34, 26 };

		assert(calc.m == value.m);
}

fn void test_mat2_inverse()
{
		Matrix2 a = { 3, 5, 6, 2 };
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
