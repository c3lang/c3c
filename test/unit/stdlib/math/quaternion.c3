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
	Quaternion rotation = quaternion::IDENTITY;
	Quaternionf rotation_f = quaternion::IDENTITY;
	assert(rotation.v == { 0, 0, 0, 1 });
	assert(rotation.v == { 0, 0, 0, 1 });
}

fn void test_rotation_identity() @if(!$feature(OLD_MATRIX))
{
	Quaternion rotation = quaternion::IDENTITY;
	Matrix4 identity_matrix = matrix::IDENTITY4;

	Quaternionf rotation_f = quaternion::IDENTITY;
	Matrix4f identity_matrix_f = matrix::IDENTITY4;

	assert(rotation.to_matrix().m.equals(identity_matrix.m));
	assert(rotation_f.to_matrixf().m.equals(identity_matrix_f.m));
}

fn void test_quaternion_to_from_axis()
{
	Quaternionf f = { 1, 2, 3, -4 };
	float[<4>] axis_angle = f.to_axis_angle();
	Quaternionf f2 = quaternion::from_axis_angle{float}(axis_angle.xyz, axis_angle.w);
	assert(f2.equals(f.normalize()));
}

fn void test_quaternion_to_from_euler()
{
	Quaternionf f = { 1, 2, 3, -4 };
	f = f.normalize();
	float[<3>] angles = f.to_euler();
	assert(angles.equals({0.19739556f, -0.82321198f, -1.37340077f}));
	Quaternionf f2 = quaternion::from_euler{float}(...angles);
	assert(f2.equals(-f));
	float[<3>] angles2 = f2.to_euler();
	assert(angles2.equals(angles));
}

fn void test_to_matrix()
{
	Matrix4 result = {
		.m00 = 0.428571, .m01 = -0.285714, .m02 = 0.857143,
		.m10 = 0.857143, .m11 = 0.428571, .m12 = -0.285714,
		.m20 = -0.285714, .m21 = 0.857143, .m22 = 0.428571,
		.m33 = 1.0
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