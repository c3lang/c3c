module math_quaternion @test;
import std::math;

fn void test()
{
	{|
		Quaternion rotation = QUATERNION_IDENTITY;
        Quaternionf rotation_f = QUATERNIONF_IDENTITY;
        assert(rotation.v == {0,0,0,1});
        assert(rotation.v == {0,0,0,1});
	|};

    {|
		Quaternion rotation = QUATERNION_IDENTITY;
        Matrix4 identity_matrix = MATRIX4_IDENTITY;

        Quaternionf rotation_f = QUATERNIONF_IDENTITY;
        Matrix4f identity_matrix_f = MATRIX4F_IDENTITY;

        assert((double[<16>])rotation.to_matrix().m == (double[<16>])identity_matrix.m);
        assert((float[<16>])rotation_f.to_matrixf().m == (float[<16>])identity_matrix_f.m);
	|};

    {|
        Matrix4 result = {
            0.428571, -0.285714, 0.857143, 0.000000, 
            0.857143, 0.428571, -0.285714, 0.000000, 
            -0.285714, 0.857143, 0.428571, 0.000000, 
            0.000000, 0.000000, 0.000000, 1.000000
        };
        
        Matrix4 rotation = Quaternion {0.5, 0.5, 0.5, 1}.to_matrix();
        Matrix4f rotation_f = Quaternionf {0.5, 0.5, 0.5, 1}.to_matrixf();

        assert(math::round_to_decimals((double[<16>])result.m, 2) == math::round_to_decimals((double[<16>])rotation.m, 2));
        assert(math::round_to_decimals((float[<16>])result.m, 2) == math::round_to_decimals((float[<16>])rotation_f.m, 2));
    |};
}