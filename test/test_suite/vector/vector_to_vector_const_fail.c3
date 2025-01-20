fn int main(String[] args)
{
	const double[4] ONE = { 1.0, 1.0, 1.0, 1.0 };
	float[<4>] vec4 = ONE; // #error: Implicitly casting 'double[4]' to 'float[<4>]'
}