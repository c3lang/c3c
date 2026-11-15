module test;

enum Foo { A, B, C }

// An enum converts to an int with an explicit cast (or with .ordinal), and the int to enum direction
// already says so. The error for the implicit enum to int conversion must not claim it is impossible,
// it must point at the explicit cast. The matched text is just "explicit cast".
fn void enum_value_to_int()
{
	Foo f = B;
	int y = f; // #error: explicit cast
}

fn void enum_constant_to_int()
{
	int y = Foo.B; // #error: explicit cast
}

// The same when the integer is not the type the enum is stored as: it says "implicitly" there too,
// the explicit cast is what makes it possible.
fn void enum_value_to_other_int_type()
{
	Foo f = B;
	long y = f; // #error: not possible to implicitly cast the enum 'Foo' to 'long'
}

// These already work, and must keep working.
fn void int_to_enum_hint()
{
	Foo f = 1; // #error: explicit cast
}

fn void explicit_cast_to_int()
{
	Foo f = B;
	int y = (int)f;
}

fn void ordinal_to_int()
{
	Foo f = B;
	int y = f.ordinal;
}
