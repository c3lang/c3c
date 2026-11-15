module test;

// float16 and bfloat are both 16 bits wide but neither contains the other: float16 has more
// precision, bfloat more range. Converting between them loses information in both directions,
// so it is not a widening and needs an explicit cast.
fn void float16_to_bfloat(float16 h)
{
	bfloat b = h; // #error: cannot implicitly be narrowed
}

fn void bfloat_to_float16(bfloat b)
{
	float16 h = b; // #error: cannot implicitly be narrowed
}

fn void float16_vector_to_bfloat_vector(float16[<4>] h)
{
	bfloat[<4>] b = h; // #error: explicit cast
}

fn void bfloat_vector_to_float16_vector(bfloat[<4>] b)
{
	float16[<4>] h = b; // #error: explicit cast
}

// These are real widenings, and must keep working.
fn void widening(float16 h, bfloat b)
{
	float f = h;
	float g = b;
	double d = h;
	double e = b;
}

fn void widening_vectors(float16[<4>] h, bfloat[<4>] b)
{
	float[<4>] f = h;
	float[<4>] g = b;
}
