fn void main()
{
	float x;
	x = $$fma(x, x, x);
	float? y;
	y = $$fma(x, x, y);
	x = $$fma(x, x, y); // #error: 'float?'
}