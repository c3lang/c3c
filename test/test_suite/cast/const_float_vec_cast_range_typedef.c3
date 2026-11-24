typedef MyU = char;
typedef MyC = ichar;
typedef MyF = float;
const float[] FS = { -1.5, 300.7 };
const float[] OK = { -1.5, 100.7 };
const MyF[] MFS = { (MyF)2.5, (MyF)400.0 };
const MyF[] MFS_OK = { (MyF)2.5, (MyF)255.9 };
const double[] EDGE = { -128.9, 127.9 };

fn void typedef_unsigned()
{
	MyU[<2>] x = (MyU[<2>])(double[<2>]){ -1.5, 3.7 }; // #error: out of range
}

fn void typedef_signed()
{
	MyC[<2>] x = (MyC[<2>])(float[<2>]){ -1.5, 300.7 }; // #error: out of range
}

fn void const_slice()
{
	ichar[<2>] x = (ichar[<2>])FS; // #error: out of range
}

fn void typedef_float_slice()
{
	char[<2>] x = (char[<2>])MFS; // #error: out of range
}

fn void typedef_float_source()
{
	char[<2>] x = (char[<2>])(MyF[<2>]){ (MyF)1.0, (MyF)256.0 }; // #error: out of range
}

fn void valid()
{
	const MyU[<2>] A = (MyU[<2>])(double[<2>]){ 1.5, 3.7 };
	const MyC[<2>] B = (MyC[<2>])(float[<2>]){ -1.5, 100.7 };
	const ichar[<2>] C = (ichar[<2>])OK;
	const char[<2>] D = (char[<2>])MFS_OK;
	const char[<2>] E = (char[<2>])(MyF[<2>]){ (MyF)1.0, (MyF)255.0 };
	const ichar[<2>] F = (ichar[<2>])EDGE;
	const MyU[<2>] G = (MyU[<2>])(double[<2>]){ -0.9, 255.9 };
	const MyC[<2>] H = (MyC[<2>])EDGE;
	$assert (int)A[0] == 1 && (int)A[1] == 3;
	$assert (int)B[0] == -1 && (int)B[1] == 100;
	$assert C[0] == -1 && C[1] == 100;
	$assert D[0] == 2 && D[1] == 255;
	$assert E[0] == 1 && E[1] == 255;
	$assert F[0] == -128 && F[1] == 127;
	$assert (int)G[0] == 0 && (int)G[1] == 255;
	$assert (int)H[0] == -128 && (int)H[1] == 127;
}
