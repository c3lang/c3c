typedef Foo = double[];

fn void main()
{
	Foo x = { 1.0, 4.5, 7.123 };
	Foo y = x;
	Foo z = x[0:2];
	Foo w = x[1..2];
	double[] yekf = x[1..1]; // #error: 'Foo' to 'double[]'
}
