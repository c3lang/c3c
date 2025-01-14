module math_tests;
import std::math;

fn void test_abs() @test
{
	int x = -21;
	assert(math::abs(x) == 21);
	double y = -123.0;
	assert(math::abs(y) == 123.0);
	float z = -21.0f;
	assert(math::abs(z) == 21.0f);
	$assert @typeis(math::abs(z), float);
	int[<3>] xx = { -1, -1000, 1000 };
	assert(math::abs(xx) == int[<3>] { 1, 1000, 1000 });
	double[<3>] yy = { -1, -0.5, 1000 };
	assert(math::abs(yy) == double[<3>] { 1, 0.5, 1000 });
}

fn void test_acos() @test
{
	int [<5>] in = { 231, -231, 1, 0, -1 };
	double [<3>] out = { 0., math::PI_2, math::PI };
	double [<6>] in2 = { 0.9, 0.6, 0.1, -0.1, -0.6, -0.9 };
	double [<6>] out2 = { 0.45102681179626236, 0.9272952180016123, 1.4706289056333368, 1.6709637479564565, 2.214297435588181, 2.6905658417935308 };
	assert(@typeis(math::acos(in[0]), double));
	assert(@typeis(math::acos((float)in[0]), float));
	assert(@typeis(math::acos((double)in[0]), double));
	for (int i = 0; i < 2; i++)
	{
		double x = math::acos(in[i]);
		assert(math::is_nan(x), "acos(%d)=%f is not nan", in[i], x);
		float f = math::acos((float)in[i]);
		assert(math::is_nan(f), "acos(%f)=%f is not nan", in[i], f);
		x = math::acos((double)in[i]);
		assert(math::is_nan(x), "acos(%f)=%f is not nan", in[i], x);
	}
	for (int i = 2; i < 5; i++)
	{
		int ii = i-2;
		double x = math::acos(in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "acos(%d)=%f is not equal to %f", in[i], x, out[ii]);
		float f = math::acos((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[ii], 1e-6), "acos(%f)=%f is not equal to %f", in[i], f, out[ii]);
		x = math::acos((double)in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "acos(%f)=%f is not equal to %f", in[i], x, out[ii]);
	}
	for (int i = 0; i < 6; i++)
	{
		float f = math::acos((float)in2[i]);
		assert(math::is_approx(f, (float)out2[i], 1e-6), "acos(%f)=%f is not equal to %f", (float)in2[i], f, (float)out2[i]);
		double x = math::acos(in2[i]);
		assert(math::is_approx(x, out2[i], 1e-12), "acos(%f)=%f is not equal to %f", in2[i], x, out2[i]);
	}
}


fn void test_acosh() @test
{
	int [<5>] in = { 0, -1, 1, 2, 231 };
	double [<3>] out = { 0., 1.3169578969248166, 6.135560205979194 };
	assert(@typeis(math::acosh(in[0]), double));
	assert(@typeis(math::acosh((float)in[0]), float));
	assert(@typeis(math::acosh((double)in[0]), double));
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_nan(math::acosh(in[i])), "acosh(%d)=%f is not nan", in[i]);
		assert(math::is_nan(math::acosh((float)in[i])), "acosh(%f) is not nan", in[i]);
		assert(math::is_nan(math::acosh((double)in[i])), "acosh(%f) is not nan", in[i]);
	}
	for (int i = 2; i < 5; i++)
	{
		int ii = i-2;
		double x = math::acosh(in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "acosh(%d)=%f is not equal to %f", in[i], x, out[ii]);
		float f = math::acosh((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[ii], 1e-6), "acosh(%f)=%f is not equal to %f", in[i], f, out[ii]);
		x = math::acosh((double)in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "acosh(%f)=%f is not equal to %f", in[i], x, out[ii]);
	}
}

fn void test_asin() @test
{
	int [<5>] in = { 231, -231, 1, 0, -1 };
	double [<3>] out = { math::PI_2, 0., -math::PI_2 };
	double [<6>] in2 = { 0.98, 0.6, 0.1, -0.1, -0.6, -0.98 };
	double [<6>] out2 = { 1.3704614844717768, 0.6435011087932844, 0.1001674211615598, -0.1001674211615598, -0.6435011087932844, -1.3704614844717768 };
	assert(@typeis(math::asin(in[0]), double));
	assert(@typeis(math::asin((float)in[0]), float));
	assert(@typeis(math::asin((double)in[0]), double));
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_nan(math::asin(in[i])), "asin(%d)=%f is not nan", in[i]);
		assert(math::is_nan(math::asin((float)in[i])), "asin(%f) is not nan", in[i]);
		assert(math::is_nan(math::asin((double)in[i])), "asin(%f) is not nan", in[i]);
	}
	for (int i = 2; i < 5; i++)
	{
		int ii = i-2;
		double x = math::asin(in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "asin(%d)=%f is not equal to %f", in[i], x, out[ii]);
		float f = math::asin((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[ii], 1e-6), "asin(%f)=%f is not equal to %f", in[i], f, out[ii]);
		x = math::asin((double)in[i]);
		assert(math::is_approx_rel(x, out[ii], 1e-12), "asin(%f)=%f is not equal to %f", in[i], x, out[ii]);
	}
	for (int i = 0; i < 6; i++)
	{
		float f = math::asin((float)in2[i]);
		assert(math::is_approx(f, (float)out2[i], 1e-6), "asin(%f)=%f is not equal to %f", (float)in2[i], f, (float)out2[i]);
		double x = math::asin(in2[i]);
		assert(math::is_approx(x, out2[i], 1e-12), "asin(%f)=%f is not equal to %f", in2[i], x, out2[i]);
	}
}

fn void test_asinh() @test
{
	int [<5>] in = { 231, 1, 0, -1, -231 };
	double [<5>] out = { 6.135569576118435, 0.881373587019543, 0., -0.881373587019543, -6.135569576118435 };
	assert(@typeis(math::asinh(in[0]), double));
	assert(@typeis(math::asinh((float)in[0]), float));
	assert(@typeis(math::asinh((double)in[0]), double));
	for (int i = 0; i < 5; i++)
	{	
		double x = math::asinh(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "asinh(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::asinh((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "asinh(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::asinh((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "asinh(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
}

fn void test_atan() @test
{
	int [<9>] in = { 231, 3, 2, 1, 0, -1, -2, -3, -231 };
	double [<9>] out = { 1.5664673495078372, 1.2490457723982544, 1.1071487177940904, math::PI_4, 0., -math::PI_4, -1.1071487177940904, -1.2490457723982544, -1.5664673495078372 };
	double [<6>] in2 = { 0.6, 0.4, 0.1, -0.1, -0.4, -0.6 };
	double [<6>] out2 = { 0.5404195002705842, 0.3805063771123649, 0.09966865249116204, -0.09966865249116204, -0.3805063771123649, -0.5404195002705842 };
	assert(@typeis(math::atan(in[0]), double));
	assert(@typeis(math::atan((float)in[0]), float));
	assert(@typeis(math::atan((double)in[0]), double));
	for (int i = 0; i < 9; i++)
	{
		double x = math::atan(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "atan(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::atan((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "atan(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::atan((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "atan(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
	for (int i = 0; i < 6; i++)
	{
		float f = math::atan((float)in2[i]);
		assert(math::is_approx(f, (float)out2[i], 1e-6), "atan(%f)=%f is not equal to %f", (float)in2[i], f, (float)out2[i]);
		double x = math::atan(in2[i]);
		assert(math::is_approx(x, out2[i], 1e-12), "atan(%f)=%f is not equal to %f", in2[i], x, out2[i]);
	}
}

fn void test_atanh() @test
{
	int [<4>] in = { 231, -231, 1, -1 };
	double [<6>] in2 = {0.8, 0.5, 0.3, -0.3, -0.5, -0.8 };
	double [<6>] out = { 1.0986122886681098, 0.5493061443340548, 0.30951960420311175, -0.30951960420311175, -0.5493061443340548, -1.0986122886681098 };
	assert(@typeis(math::atanh(in[0]), double));
	assert(@typeis(math::atanh((float)in[0]), float));
	assert(@typeis(math::atanh((double)in[0]), double));
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_nan(math::atanh(in[i])), "atanh(%d) is not nan", in[i]);
		assert(math::is_nan(math::atanh((float)in[i])), "atanh(%f) is not nan", in[i]);
		assert(math::is_nan(math::atanh((double)in[i])), "atanh(%f) is not nan", in[i]);
	}
	for (int i = 2; i < 4; i++)
	{
		assert(math::is_inf(math::atanh(in[i])), "atanh(%d)=%f is not inf", in[i]);
		assert(math::is_inf(math::atanh((float)in[i])), "atanh(%f) is not inf", in[i]);
		assert(math::is_inf(math::atanh((double)in[i])), "atanh(%f) is not inf", in[i]);
	}
	assert(math::atanh(0) == 0., "atanh(%d) is not equal to %f", 0, 0.);
	assert(math::atanh(0.f) == 0.f, "atanh(%f) is not equal to %f", 0.f, 0.f);
	assert(math::atanh(0.) == 0., "atanh(%f) is not equal to %f", 0., 0.);
	for (int i = 0; i < 6; i++)
	{
		float f = math::atanh((float)in2[i]);
		assert(math::is_approx(f, (float)out[i], 1e-6), "atanh(%f)=%f is not equal to %f", in2[i], f, out[i]);
		double x = math::atanh((double)in2[i]);
		assert(math::is_approx(x, out[i], 1e-12), "atanh(%f)=%f is not equal to %f", in2[i], x, out[i]);
	}
}

fn void test_floating_point_word() @test
{
	float f = 1.f;
	assert(f.word() == 0x3f800000);
	// xor swap
	float f1 = 2.f;
	float f2 = 3.f;
	uint u1 = f1.word();
	uint u2 = f2.word();
	u1 ^= u2; u2 ^= u1; u1 ^= u2;
	f1.set_word(u1);
	f2.set_word(u2);
	assert((f1 == 3.f) && (f2 == 2.f));
	// sign bit trick
	f = -1.f;
	assert((f.word() >> 31) == 1);
	f = 1.f;
	assert((f.word() >> 31) == 0); 
	// absolute value bit trick
	float[<4>] fvals = { 1.f, -1.f, 91.5f, -91.5f };
	for (int i = 0; i < 4; i++)
	{
		f = fvals[i];
		f.set_word(f.word() & 0x7fffffff);
		assert(f == math::abs(fvals[i]));
	}

	double d = 1.;
	assert((d.high_word() - 0x3ff00000 | d.low_word()) == 0);
	// xor swap
	double d1 = 2.0;
	double d2 = 3.0;
	uint u1_low = d1.low_word();
	uint u2_low = d2.low_word();
	u1_low ^= u2_low; u2_low ^= u1_low; u1_low ^= u2_low;
	uint u1_high = d1.high_word();
	uint u2_high = d2.high_word();
	u1_high ^= u2_high; u2_high ^= u1_high; u1_high ^= u2_high;
	d1.set_low_word(u1_low);
	d1.set_high_word(u1_high);
	d2.set_low_word(u2_low);
	d2.set_high_word(u2_high);
	assert((d1 == 3.) && (d2 == 2.));
	// sign bit trick
	d = -1.;
	assert((d.high_word() >> 31) == 1);
	d = 1.;
	assert((d.high_word() >> 31) == 0);
	// absolute value bit trick
	double[<4>] vals = { 1., -1., 91.5, -91.5 };
	for (int i = 0; i < 4; i++)
	{
		d = vals[i];
		d.set_high_word(d.high_word() & 0x7fffffff);
		assert(d == math::abs(vals[i]));
	}
}

fn void test_ceil() @test
{
	double d = -123.1;
	assert(math::ceil(d) == -123.0);
	d = 123.1;
	assert(math::ceil(d) == 124.0);
	d = 0.1;
	assert(math::ceil(d) == 1);
	d = -0.9;
	assert(math::ceil(d) == 0);
	$assert @typeis(math::ceil(d), double);
	float f = -123.1f;
	assert(math::ceil(f) == -123.0f);
	f = 123.1f;
	assert(math::ceil(f) == 124.0f);
	f = 0.1f;
	assert(math::ceil(f) == 1.0f);
	f = -0.9f;
	assert(math::ceil(f) == 0.0f);
	$assert @typeis(math::ceil(f), float);
	double[<5>] vec = { -123.1, 123.1, 0.1, -0.9, 0 };
	assert(math::ceil(vec) == double[<5>] { -123, 124, 1, 0, 0 });
}

fn void test_cos() @test
{
	int [<5>] in = { 231, 1, 0, -1, -231 };
	double [<5>] out = { 0.09280621889587707, 0.54030230586813972 , 1., 0.54030230586813972, 0.09280621889587707 };
	float [<2>] in2 = { math::PI, 0.f };
	float [<2>] out2 =  { -1.f, 1.f };
	double [<2>] in3 = { math::PI, 0. };
	double [<2>] out3 =  { -1., 1. };
	assert(@typeis(math::cos(in[0]), double));
	assert(@typeis(math::cos((float)in[0]), float));
	assert(@typeis(math::cos((double)in[0]), double));
	for (int i = 0; i < 5; i++)
	{	
		double x = math::cos(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "cos(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::cos((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "cos(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::cos((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "cos(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
	float [<2>] vecf = math::cos(in2);
	double [<2>] vec = math::cos(in3);
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_approx_rel(vecf[i], out2[i], 1e-6), "cos(%f)=%f is not equal to %f", in2[i], vecf[i], out2[i]);
		assert(math::is_approx_rel(vec[i], out3[i], 1e-12), "cos(%f)=%f is not equal to %f", in3[i], vec[i], out3[i]);
	}
}

fn void test_exp() @test
{
	int[<5>] in = { 2, 1, 0, -1, -2 };
	double[<5>] out = { 7.38905609893065, math::E , 1., 0.36787944117144233, 0.1353352832366127 };
	float[<6>] in2 = { 1.8f, 0.6f, 0.4f, -0.4f, -0.8f, -1.8f };
	float[<6>] out2 =  {6.049647464412946f, 1.8221188003905089f, 1.4918246976412703f, 0.6703200460356393f, 0.44932896411722156f, 0.16529888822158656f };
	double[<6>] in3 = { 1.8, 0.6, 0.4, -0.4, -0.8, -1.8 };
	double[<6>] out3 =  {6.049647464412946, 1.8221188003905089, 1.4918246976412703, 0.6703200460356393, 0.44932896411722156, 0.16529888822158656 };
	assert(@typeis(math::exp(in[0]), double));
	assert(@typeis(math::exp((float)in[0]), float));
	assert(@typeis(math::exp((double)in[0]), double));
	for (int i = 0; i < 5; i++)
	{	
		double x = math::exp(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "exp(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::exp((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "exp(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::exp((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "exp(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
	float[<6>] vecf = math::exp(in2);
	double[<6>] vec = math::exp(in3);
	for (int i = 0; i < 6; i++)
	{
		assert(math::is_approx_rel(vecf[i], out2[i], 1e-6), "exp(%f)=%f is not equal to %f", in2[i], vecf[i], out2[i]);
		assert(math::is_approx_rel(vec[i], out3[i], 1e-12), "exp(%f)=%f is not equal to %f", in3[i], vec[i], out3[i]);
	}
}

fn void test_floor() @test
{
	double d = -123.1;
	assert(math::floor(d) == -124.0);
	d = 123.1;
	assert(math::floor(d) == 123.0);
	d = 0.9;
	assert(math::floor(d) == 0);
	d = -0.1;
	assert(math::floor(d) == -1);
	$assert @typeis(math::floor(d), double);
	float f = -123.1f;
	assert(math::floor(f) == -124.0f);
	f = 123.1f;
	assert(math::floor(f) == 123.0f);
	f = 0.9f;
	assert(math::floor(f) == 0.0f);
	f = -0.1f;
	assert(math::floor(f) == -1.0f);
	$assert @typeis(math::floor(f), float);
	double[<5>] vec = { -123.1, 123.1, 0.9, -0.1, 0 };
	assert(math::floor(vec) == double[<5>] { -124, 123, 0, -1, 0 });
}

fn void test_log() @test
{
	int[<8>] in = { 1, 10, 100, 1000, 1, 4, 8, 16 };
	double[<8>] out = { 0., 1., 2., 3., 0., 2. / 3., 1., 4. / 3. };
	float[<4>] bf = { 1.f / math::E, 1.f / (math::E * math::E), 1.f / (math::E * math::E), 1.f / math::E };
	float[<4>] in2 = { math::E * math::E, math::E, 1.f / math::E, 1.f / (math::E * math::E) };
	float[<4>] out2 =  { -2.f, -0.5f, 0.5f, 2.f };
	double[<4>] bx = { 1. / math::E, 1. / (math::E * math::E), 1. / (math::E * math::E), 1. / math::E };
	double[<4>] in3 = { math::E * math::E, math::E, 1. / math::E, 1. / (math::E * math::E) };
	double[<4>] out3 =  { -2., -0.5, 0.5, 2. };
	assert(@typeis(math::log(in[0], in[0]), double));
	assert(@typeis(math::log(in[0], (float)in[0]), float));
	assert(@typeis(math::log((float)in[0], in[0]), float));
	assert(@typeis(math::log(in[0], (double)in[0]), double));
	assert(@typeis(math::log((double)in[0], in[0]), double));
	assert(@typeis(math::log((float)in[0], (float)in[0]), float));
	assert(@typeis(math::log((float)in[0], (double)in[0]), double));
	assert(@typeis(math::log((double)in[0], (float)in[0]), double));
	assert(@typeis(math::log((double)in[0], (double)in[0]), double));
	for (int i = 0; i < 8; i++)
	{
		int base = (i < 4) ? 10 : 8;
		
		double x = math::log(in[i], base);
		assert(math::is_approx_rel(x, out[i], 1e-12), "log(%d,%d)=%f is not equal to %f", in[i], base, x, out[i]);
		
		float f = math::log((float)in[i], base);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "log(%f,%d)=%f is not equal to %f", in[i], base, f, out[i]);

		x = math::log((double)in[i], base);
		assert(math::is_approx_rel(x, out[i], 1e-12), "log(%f,%d)=%f is not equal to %f", in[i], base, x, out[i]);
	}
	float[<4>] vecf = math::log(in2, bf);
	double[<4>] vec = math::log(in3, bx);
	for (int i = 0; i < 4; i++)
	{
		assert(math::is_approx_rel(vecf[i], out2[i], 1e-6), "log(%f,%f)=%f is not equal to %f", in2[i], bf[i], vecf[i], out2[i]);
		assert(math::is_approx_rel(vec[i], out3[i], 1e-12), "log(%f,%f)=%f is not equal to %f", in3[i], bx[i], vec[i], out3[i]);
	}
}

fn void test_pow() @test
{
	int[<10>] e =  { 2, 1, 0, -1, -2, 2, 1, 0, -1, -2 };
	double[<10>] out = { 100., 10., 1., 0.1, 0.01, 4.,  2., 1., 0.5, 0.25 };
	float[<2>] base2 = { -1.f / math::E, 1.f / math::E };
	float[<5>] out2 = { 1.f / (math::E * math::E), 1.f / math::E, 1.f, math::E, math::E * math::E };
	double[<2>] base3 = { -1. / math::E, 1. / math::E };
	double[<5>] out3 = { 1. / (math::E * math::E), 1. / math::E, 1., math::E, math::E * math::E };
	assert(@typeis(math::pow(e[1], e[1]), double));
	assert(@typeis(math::pow((float)e[1], e[1]), float));
	assert(@typeis(math::pow((double)e[1], e[1]), double));
	for (int i = 0; i < 10; i++)
	{
		for (int j = 0; j < 2; j++)
		{
			int base = (2 * j - 1) * ((i < 5) ? 10 : 2);
			
			double x = math::pow(base, e[i]);
			double outx = (e[i] & 1) ? (double)(2 * j - 1) * out[i] : out[i];
			assert(math::is_approx_rel(x, outx, 1e-12), "pow(%d,%d)=%f is not equal to %f", base, e[i], x, outx);
			
			float f = math::pow((float)base, e[i]);
			float outf = (e[i] & 1) ? (float)(2 * j - 1) * (float)out[i] : (float)out[i];
		assert(math::is_approx_rel(f, outf, 1e-6), "pow(%d,%f)=%f is not equal to %f", base, e[i], f, outf);
		
			x = math::pow((double)base, e[i]);
			assert(math::is_approx_rel(x, outx, 1e-12), "pow(%d,%f)=%f is not equal to %f", base, e[i], x, outx);
		}
	}
	for (int i = 0; i < 5; i++)
	{
		float[<2>] vecf = math::pow(base2, e[i]);
		double[<2>] vec = math::pow(base3, e[i]);
		for (int j = 0; j < 2; j++)
		{
			float outf = (e[i] & 1) ? (float)(2 * j - 1) * (float)out2[i] : (float)out2[i];
			assert(math::is_approx_rel(vecf[j], outf, 1e-6), "pow(%f,%f)=%f is not equal to %f", base2[i], e[i], vecf[j], outf);
			double outx = (e[i] & 1) ? (double)(2 * j - 1) * out3[i] : out3[i];
			assert(math::is_approx_rel(vec[j], outx, 1e-12), "pow(%f,%f)=%f is not equal to %f", base3[i], e[i], vec[j], outx);
		}
	}
	
}

fn void test_sign() @test
{
	int x = -21;
	assert(math::sign(x) == -1);
	x = 238219382;
	assert(math::sign(x) == 1);
	x = 0;
	assert(math::sign(x) == 0);
	uint y = 23;
	assert(math::sign(y) == 1);
	y = 0;
	assert(math::sign(y) == 0);
	y = (uint)-21;
	assert(math::sign(y) == 1);
	assert(@typeis(math::sign(y), uint));
}


fn void test_sin() @test
{
	int [<5>] in = { 231, 1, 0, -1, -231 };
	double [<5>] out = { -0.99568418975810324, 0.84147098480789651 , 0., -0.84147098480789651, 0.99568418975810324 };
	float [<2>] in2 = { math::PI_2, -math::PI_2 };
	float [<2>] out2 =  { 1.f, -1.f };
	double [<2>] in3 = { math::PI_2, -math::PI_2 };
	double [<2>] out3 =  { 1., -1. };
	assert(@typeis(math::sin(in[0]), double));
	assert(@typeis(math::sin((float)in[0]), float));
	assert(@typeis(math::sin((double)in[0]), double));
	for (int i = 0; i < 5; i++)
	{	
		double x = math::sin(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "sin(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::sin((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "sin(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::sin((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "sin(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
	float [<2>] vecf = math::sin(in2);
	double [<2>] vec = math::sin(in3);
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_approx_rel(vecf[i], out2[i], 1e-6), "sin(%f)=%f is not equal to %f", in2[i], vecf[i], out2[i]);
		assert(math::is_approx_rel(vec[i], out3[i], 1e-12), "sin(%f)=%f is not equal to %f", in3[i], vec[i], out3[i]);
	}
}

fn void test_tan() @test
{
	int [<5>] in = { 231, 1, 0, -1, -231 };
	double [<5>] out = { -10.7286365246191129, 1.5574077246549022 , 0., -1.5574077246549022, 10.7286365246191129 };
	float [<2>] in2 = { math::PI_4, -math::PI_4 };
	float [<2>] out2 =  { 1.f, -1.f };
	double [<2>] in3 = { math::PI_4, -math::PI_4 };
	double [<2>] out3 =  { 1., -1. };
	assert(@typeis(math::tan(in[0]), double));
	assert(@typeis(math::tan((float)in[0]), float));
	assert(@typeis(math::tan((double)in[0]), double));
	for (int i = 0; i < 5; i++)
	{	
		double x = math::tan(in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "tan(%d)=%f is not equal to %f", in[i], x, out[i]);
		float f = math::tan((float)in[i]);
		assert(math::is_approx_rel(f, (float)out[i], 1e-6), "tan(%f)=%f is not equal to %f", in[i], f, out[i]);
		x = math::tan((double)in[i]);
		assert(math::is_approx_rel(x, out[i], 1e-12), "tan(%f)=%f is not equal to %f", in[i], x, out[i]);
	}
	float [<2>] vecf = math::tan(in2);
	double [<2>] vec = math::tan(in3);
	for (int i = 0; i < 2; i++)
	{
		assert(math::is_approx_rel(vecf[i], out2[i], 1e-6), "tan(%f)=%f is not equal to %f", in2[i], vecf[i], out2[i]);
		assert(math::is_approx_rel(vec[i], out3[i], 1e-12), "tan(%f)=%f is not equal to %f", in3[i], vec[i], out3[i]);
	}
}

fn void test_trunc() @test
{
	double d = -123.9;
	assert(math::trunc(d) == -123.0);
	d = 123.1;
	assert(math::trunc(d) == 123.0);
	d = 0.9;
	assert(math::trunc(d) == 0);
	d = -0.9;
	assert(math::trunc(d) == 0);
	$assert @typeis(math::trunc(d), double);
	float f = -123.9f;
	assert(math::trunc(f) == -123.0f);
	f = 123.9f;
	assert(math::trunc(f) == 123.0f);
	f = 0.9f;
	assert(math::trunc(f) == 0.0f);
	f = -0.9f;
	assert(math::trunc(f) == -0.0f);
	$assert @typeis(math::trunc(f), float);
	double[<5>] vec = { -123.9, 123.9, 0.9, -0.9, 0 };
	assert(math::trunc(vec) == double[<5>] { -123, 123, 0, 0, 0 });
}

fn void test_round_decimals() @test
{
	double d = 0.532451241142;
	float d_f = 0.532451241142;
	assert(math::round_to_decimals(d, 2) == 0.53);
	assert(math::round_to_decimals(d, 5) == 0.53245);

	assert(math::round_to_decimals(d_f, 2) == 0.53f);
	assert(math::round_to_decimals(d_f, 5) == 0.53245f);
}

fn void test() @test
{
	double radians = math::deg_to_rad(45);
	float radians_f = (float)math::deg_to_rad(45);

	assert(math::round_to_decimals(radians, 3) == 0.785);
	assert(math::round_to_decimals(radians_f, 3) == 0.785f);
}

fn void test_muldiv()
{
	char a = 20;
	assert(a.muldiv(20, 10) == 40);
	ichar b = 20;
	assert(b.muldiv(20, -10) == -40);
	short c = 16000;
	assert(c.muldiv(4, 2) == 32000);
	ushort d = 16000;
	assert(d.muldiv(8, 2) == 64000);
	int e = 1_000_000;
	assert(e.muldiv(40000, 10000) == 4_000_000);
	uint f = 3_000_000_000u;
	assert(f.muldiv(110, 100) == 3_300_000_000u);
	long g = 1_000_000_000_000i64;
	assert(g.muldiv(2_000_000_000_000i64, 1_000_000_000i64) == 2_000_000_000_000_000i64);
	ulong h = 1_000_000_000_000u64;
	assert(h.muldiv(2_000_000_000_000u64, 1_000_000_000u64) == 2_000_000_000_000_000u64);

	char[<4>] i = {20, 30, 40, 50};
	assert(i.muldiv(12,10) == char[<4>] {24, 36, 48, 60});
	assert(i.muldiv(char[<4>]{11, 12, 13, 14}, char[<4>]{10,10,10,10}) == char[<4>]{22, 36, 52, 70});

	long[<4>] j = {1_000_000_000_000i64, 2_000_000_000_000i64, 3_000_000_000_000i64, 4_000_000_000_000i64};
	assert(j.muldiv(2_000_000_000_000i64, 1_000_000_000i64) == long[<4>]{2_000_000_000_000_000i64, 4_000_000_000_000_000i64, 6_000_000_000_000_000i64, 8_000_000_000_000_000i64});
	
	ichar[<4>] k = {20, 30, 40, 50};
	assert(k.muldiv(20,-10) == ichar[<4>]{-40,-60,-80,-100});
}

fn void test_gcd() @test
{
	assert(math::gcd(20,15) == 5);
	assert(math::gcd(15,20) == 5);
	assert(math::gcd(-15,20) == 5);
	assert(math::gcd(15,-20) == 5);
	assert(math::gcd(-15,-20) == 5);
	assert(math::gcd(5,15,20) == 5);
	assert(math::gcd(1,2,3) == 1);
	assert(math::gcd(2,4,6,8) == 2);
}

fn void test_lcm() @test
{
	assert(math::lcm(4,5) == 20);
	assert(math::lcm(6,10) == 30);
	assert(math::lcm(-8,20) == 40);
	assert(math::lcm(8,-20) == 40);
	assert(math::lcm(-8,-20) == 40);
	assert(math::lcm(11,17) == 11*17);
	assert(math::lcm(11,17,227,263) == 11*17*227*263);
}

