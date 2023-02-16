module std::math;

fn float __roundevenf(float f) @extern("roundevenf") @weak @nostrip
{
	// Slow implementation
	return round(f / 2) * 2;
}

fn double __roundeven(double d) @extern("roundeven") @weak @nostrip
{
	// Slow implementation
	return round(d / 2) * 2;
}
