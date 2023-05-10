module math_matrix @test;
import std::math;

fn void test_mat4()
{
	{|
		Matrix4 mat = math::matrix4_identity();
		Matrix4 mat2 = { 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1 };
		Matrix4 calc = mat.mul(mat2);
		assert(calc.m == mat.m);

		Matrix4 translated = mat.translate(Vec3{0.0, 0.0, 0.0});
		assert(translated.m == mat.m);
	|};

	{|
		Matrix4 mat = { 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8 };
		Matrix4 mat2 = { 8, 7, 6, 5, 4, 3, 2, 1, 8, 7, 6, 5, 4, 3, 2, 1 };
		Matrix4 calc = mat.mul(mat2);
		Matrix4 value = { 56, 46, 36, 26, 152, 126, 100, 74, 56, 46, 36, 26, 152, 126, 100, 74 };
		assert(calc.m == value.m);
	|};
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

fn void test_vec3()
{
	Vec3 cross = Vec3{2,3,4}.cross(Vec3{5,6,7});
	assert(cross == Vec3{-3,6,-3});
}


// Todo: how to assert when result has long decimal value
// fn void quaternion_to_matrix()
// {
// 	Quaternion rotation = { 0.259, 0.0, 0.0, 0.966 };
// 	Matrix4 result = rotation.to_matrix();
// 	Matrix4 correct = {1.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.865870, 0.500269, 0.000000, 0.000000, -0.500269, 0.865870, 0.000000, 0.000000, 0.000000, 0.000000, 1.000000};
// 	assert(result.m == correct.m);
// }