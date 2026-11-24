module const_float_vec_cast_in_range;

typedef MyU = char;
typedef MyC = ichar;
typedef MyF = float;

const float[] FS = { -1.5, 100.7 };
const MyF[] MFS = { (MyF)2.5, (MyF)255.9 };
const double[] EDGE = { -128.9, 127.9 };

fn void typedef_elements_in_range() @test
{
	MyU[<2>] a = (MyU[<2>])(double[<2>]){ 1.5, 3.7 };
	MyC[<2>] b = (MyC[<2>])(float[<2>]){ -1.5, 100.7 };
	MyU[<2>] c = (MyU[<2>])(double[<2>]){ -0.9, 255.9 };
	assert((int)a[0] == 1 && (int)a[1] == 3);
	assert((int)b[0] == -1 && (int)b[1] == 100);
	assert((int)c[0] == 0 && (int)c[1] == 255);
}

fn void const_slice_in_range() @test
{
	ichar[<2>] a = (ichar[<2>])FS;
	char[<2>] b = (char[<2>])MFS;
	ichar[<2>] c = (ichar[<2>])EDGE;
	MyC[<2>] d = (MyC[<2>])EDGE;
	assert(a[0] == -1 && a[1] == 100);
	assert(b[0] == 2 && b[1] == 255);
	assert(c[0] == -128 && c[1] == 127);
	assert((int)d[0] == -128 && (int)d[1] == 127);
}

fn void typedef_float_source_in_range() @test
{
	char[<2>] a = (char[<2>])(MyF[<2>]){ (MyF)1.0, (MyF)255.0 };
	assert(a[0] == 1 && a[1] == 255);
}
