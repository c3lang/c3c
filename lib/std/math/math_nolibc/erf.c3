module std::math::nolibc @if(env::NO_LIBC || $feature(C3_MATH));

<*
 Error function for float.
*>
fn float _erff(float x)
{
	// Handle special cases.
	if (x == 0.0) return 0.0;
	if (x == float.inf) return 1.0;
	if (x == -float.inf) return -1.0;

	// Use symmetry: erf(-x) = -erf(x)
	float sign = 1.0;
	if (x < 0.0)
	{
	    x = -x;
	    sign = -1.0;
	}

	const float P1 =  0.4067420160f;
	const float P2 =  0.0072279182f;
	const float A1 =  0.3168798904f;
	const float A2 = -0.1383293141f;
	const float A3 =  1.0868083034f;
	const float A4 = -1.1169415512f;
	const float A5 =  1.2064490307f;
	const float A6 = -0.3931277152f;
	const float A7 =  0.0382613542f;

	float t = 1.0f / (1.0f + P1 * x + P2 * x * x);
	float t2 = t * t;
	float sum = A1 * t + A2 * t2 + A3 * t2 * t + A4 * t2 * t2;
	sum += A5 * t2 * t2 * t + A6 * t2 * t2 * t2 + A7 * t2 * t2 * t2 * t;

	// For intermediate x, use continued fraction.
	return sign * (1.0f - sum * math::exp(-x * x));
}

<*
 Error function for double.
*>
fn double _erf(double x)
{
	// Handle special cases.
	if (x == 0.0) return 0.0;
	if (x == double.inf) return 1.0;
	if (x == -double.inf) return -1.0;

	// Use symmetry: erf(-x) = -erf(x)
	double sign = 1.0;
	if (x < 0.0)
	{
	    x = -x;
	    sign = -1.0;
	}

	const double P1 =  0.406742016006509;
	const double P2 =  0.0072279182302319;
	const double A1 =  0.316879890481381;
	const double A2 = -0.138329314150635;
	const double A3 =  1.08680830347054;
	const double A4 = -1.11694155120396;
	const double A5 =  1.20644903073232;
	const double A6 = -0.393127715207728;
	const double A7 =  0.0382613542530727;

	double t = 1.0 / (1.0 + P1 * x + P2 * x * x);
	double t2 = t * t;
	double sum = A1 * t + A2 * t2 + A3 * t2 * t + A4 * t2 * t2;
	sum += A5 * t2 * t2 * t + A6 * t2 * t2 * t2 + A7 * t2 * t2 * t2 * t;

	// For intermediate x, use continued fraction.
	return sign * (1.0 - sum * math::exp(-x * x));
}


