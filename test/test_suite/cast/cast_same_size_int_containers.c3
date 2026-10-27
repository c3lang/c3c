module test;

// Converting between a signed and an unsigned integer of the same size needs an explicit cast
// (`int` -> `uint` is an error), and so does converting a container of them: an array copies the
// elements and would silently turn -1 into 4294967295. The pointer conversions already require it.
// Each cast is in its own function, as the analysis stops at the first error in a function.
fn void array(int[3] x) { uint[3] y = x; } // #error: explicit cast
fn void array_back(uint[3] x) { int[3] y = x; } // #error: explicit cast
fn void slice(int[] x) { uint[] y = x; } // #error: explicit cast
fn void slice_back(uint[] x) { int[] y = x; } // #error: explicit cast
fn void chars(char[] x) { ichar[] y = x; } // #error: explicit cast
fn void vector_to_array(int[<4>] x) { uint[4] y = x; } // #error: explicit cast
fn void vector_to_array_back(uint[<4>] x) { int[4] y = x; } // #error: explicit cast
fn void pointers(int* x) { uint* y = x; } // #error: explicit cast
fn void array_to_vector(int[4] x) { uint[<4>] y = x; } // #error: explicit cast
fn void vector_to_vector(int[<4>] x) { uint[<4>] y = x; } // #error: explicit cast

// With an explicit cast it works.
fn void explicit_casts(int[3] a, int[] s, char[] c, int[<4>] v)
{
	uint[3] x = (uint[3])a;
	uint[] y = (uint[])s;
	ichar[] z = (ichar[])c;
	uint[4] w = (uint[4])v;
}

// Same signedness is not affected, nor is the conversion between String and char[].
fn void same_signedness(int[3] a, int[] s, int[<4>] v, String str, char[] chars)
{
	int[3] x = a;
	int[] y = s;
	int[4] w = v;
	char[] c = str;
	String d = (String)chars;
}
