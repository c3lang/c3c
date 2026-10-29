module test;

typedef Aaa = int;
typedef Bbb = String;
typedef Ccc = float;

// The error is reported once. The explicit retry used to print its own error as well,
// and the second, identical error would show up as an unexpected error here.
fn void not_castable_even_explicitly()
{
	Aaa a = (Aaa)1;
	Bbb b = a; // #error: It is not possible to cast 'Aaa' to 'Bbb'
}

// This already works: the explicit cast would be fine, so there is a single hint.
fn void castable_explicitly()
{
	Aaa a = (Aaa)1;
	Ccc c = a; // #error: explicit casts between distinct types are usually not intended
}
