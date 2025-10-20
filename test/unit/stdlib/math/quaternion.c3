module math_tests::quaternion @test;
import std::math;

fn void test_rad_to_deg()
{
	Quaternion value = { 0, 0.7071068, 0.7071068, 0 };
	double angle_radians = value.to_angle();
	assert(math::round_to_decimals(angle_radians, 2) == 3.14);
	assert(math::rad_to_deg(angle_radians) == 180.0);
}

fn void test_rotation()
{
	double[<3>] axis = { 3, 0, 3 };
	Quaternion rotation = { 0.5, 0, 0.5, 0 };
	double[<3>] result = { 1.500000, 0.000000, 1.500000 };

	assert(rotation * axis == result);
	assert(rotation.rotate_vec3(axis) == result);
	assert(axis.rotate_quat(rotation) == result);
}
fn void test_conjugate()
{
	Quaternion value = { 0.5, 0.2, 1.0, 0.5 };
	Quaternion inverse = value.conjugate();
	assert(inverse.v == { -0.500000, -0.200000, -1.000000, 0.500000 });
}

fn void test_identity()
{
	Quaternion rotation = QUATERNION_IDENTITY;
	Quaternionf rotation_f = QUATERNIONF_IDENTITY;
	assert(rotation.v == { 0, 0, 0, 1 });
	assert(rotation.v == { 0, 0, 0, 1 });
}

fn void test_rotation_identity()
{
	Quaternion rotation = QUATERNION_IDENTITY;
	Matrix4 identity_matrix = MATRIX4_IDENTITY;

	Quaternionf rotation_f = QUATERNIONF_IDENTITY;
	Matrix4f identity_matrix_f = MATRIX4F_IDENTITY;

	assert((double[<16>])rotation.to_matrix().m == (double[<16>])identity_matrix.m);
	assert((float[<16>])rotation_f.to_matrixf().m == (float[<16>])identity_matrix_f.m);
}

fn void test_to_matrix()
{
	Matrix4 result = {
		0.428571, -0.285714, 0.857143, 0.000000,
		0.857143, 0.428571, -0.285714, 0.000000,
		-0.285714, 0.857143, 0.428571, 0.000000,
		0.000000, 0.000000, 0.000000, 1.000000
	};

	Matrix4 rotation = (Quaternion) { 0.5, 0.5, 0.5, 1 }.to_matrix();
	Matrix4f rotation_f = (Quaternionf) { 0.5, 0.5, 0.5, 1 }.to_matrixf();

	assert(math::round_to_decimals((double[<16>])result.m, 2) == math::round_to_decimals((double[<16>])rotation.m, 2));
	assert(math::round_to_decimals((float[<16>])result.m, 2) == math::round_to_decimals((float[<16>])rotation_f.m, 2));
}
fn void test_normalize()
{
	Quaternionf value = quaternion::from_axis_angle({ 1, 0, 0 }, math::PI).normalize();
	assert(math::round_to_decimals(value.v, 2) == { 1, 0, 0, 0 });
}

fn void test_normalize2()
{
	Quaternionf value = quaternion::from_axis_angle({ 0, 1, 0 }, math::PI / 2).normalize();
	assert(math::round_to_decimals(value.v, 4) == { 0, 0.7071, 0, 0.7071 });
}

fn void test_mult()
{
	Quaternionf rotation = quaternion::from_axis_angle({ 0.0f, 1.0f, 0.0f }, (float)math::deg_to_rad(90.0f));
	float[<3>] rotate_point = rotation * (float[<3>]){ 1, 0, 0 };
	assert(math::round_to_decimals(rotate_point, 2) == (float[<3>]){ 0, 0, -1.0 });
}