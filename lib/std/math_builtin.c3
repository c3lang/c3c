module std::math;

fn float __roundevenf(float f) @extname("roundevenf") @weak
{
	// Slow implementation
	return round(f / 2) * 2;
}

fn double __roundeven(double d) @extname("roundeven") @weak
{
	// Slow implementation
	return round(d / 2) * 2;
}
