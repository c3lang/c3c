module test;

// A constant float is truncated towards zero when it is cast to an integer. If the truncated value
// does not fit the integer it is an error: it used to abort the backend (for the types up to 32 bits),
// or silently give a wrong value (for the 64 bit types). Each cast is in its own function, as the
// analysis stops at the first error in a function.
fn void int_too_big() { int a = (int)1e30; } // #error: out of range
fn void int_just_over() { int a = (int)2147483648.0; } // #error: out of range
fn void int_just_under() { int a = (int)-2147483649.0; } // #error: out of range
fn void char_too_big() { char a = (char)300.7; } // #error: out of range
fn void ichar_too_small() { ichar a = (ichar)-129.0; } // #error: out of range
fn void short_too_small() { short a = (short)-40000.5; } // #error: out of range
fn void uint_too_big() { uint a = (uint)4294967296.0; } // #error: out of range
fn void uint_negative() { uint a = (uint)-1.5; } // #error: out of range
fn void long_too_big() { long a = (long)1e30; } // #error: out of range
fn void long_just_over() { long a = (long)9223372036854775808.0; } // #error: out of range
fn void ulong_too_big() { ulong a = (ulong)1e30; } // #error: out of range
fn void ulong_negative() { ulong a = (ulong)-1.0; } // #error: out of range
fn void int128_too_big() { int128 a = (int128)1e39; } // #error: out of range
fn void infinity() { int a = (int)(1.0 / 0.0); } // #error: out of range
fn void negative_infinity() { int a = (int)(-1.0 / 0.0); } // #error: out of range
fn void not_a_number() { int a = (int)(0.0 / 0.0); } // #error: out of range

// The same for the elements of a constant vector or array that is cast to an integer vector.
const float[<2>] BIG = { 1e30, 1 };
fn void vector_int() { int[<2>] a = (int[<2>])(float[<2>]){ 1, 1e30 }; } // #error: out of range
fn void vector_char() { char[<2>] a = (char[<2>])(float[<2>]){ 300.5, 2 }; } // #error: out of range
fn void vector_negative() { uint[<2>] a = (uint[<2>])(float[<2>]){ 1, -1.5 }; } // #error: out of range
fn void array_to_vector() { int[<2>] a = (int[<2>])(float[2]){ 1, -1e30 }; } // #error: out of range
fn void constant_vector() { int[<2>] a = (int[<2>])BIG; } // #error: out of range

// Values that fit after truncation must keep working.
fn void fits()
{
	$assert((int)2147483647.9 == 2147483647);
	$assert((int)-2147483648.9 == int::min);
	$assert((char)255.9 == 255);
	$assert((ichar)-128.9 == -128);
	$assert((short)-32768.0 == -32768);
	$assert((uint)4294967295.9 == 4294967295);
	$assert((uint)-0.5 == 0);
	$assert((int)0.0 == 0);
	$assert((int)-0.0 == 0);
	$assert((long)-9223372036854775808.0 == long::min);
	$assert((ulong)1e19 == 10000000000000000000);
	$assert((int128)1e30 > 0);
	int[<2>] v = (int[<2>])(float[<2>]){ 1.9, -1.9 };
	char[<2>] w = (char[<2>])(float[<2>]){ 255.9, 0.5 };
}
