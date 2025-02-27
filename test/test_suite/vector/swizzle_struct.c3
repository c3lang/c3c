struct Vector4 @packed @align(4)
{
	inline float[<4>] value;
}

fn int main(String[] args)
{
	Vector4 value;
	float[<2>] x = value.xy;
	return 0;
}
