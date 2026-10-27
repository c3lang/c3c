module test;

// An inline distinct integer converts like the integer it wraps. A signed one must never convert
// implicitly to an unsigned one, also not when both have the same size (`int` -> `uint` is an
// error). Each cast is in its own function, as the analysis stops at the first error in a function.
typedef InInt = inline int;
typedef InShort = inline short;
typedef InIchar = inline ichar;
typedef InLong = inline long;
typedef InUint = inline uint;
constset CsInt : inline int { X = 1 }

fn void int_to_uint(InInt x) { uint y = x; } // #error: cannot implicitly be narrowed
fn void short_to_ushort(InShort x) { ushort y = x; } // #error: cannot implicitly be narrowed
fn void ichar_to_char(InIchar x) { char y = x; } // #error: cannot implicitly be narrowed
fn void long_to_ulong(InLong x) { ulong y = x; } // #error: cannot implicitly be narrowed
fn void constset_to_uint(CsInt x) { uint y = x; } // #error: cannot implicitly be narrowed

// These already work, and must keep working.
fn void uint_to_int(InUint x) { int y = x; } // #error: cannot implicitly be narrowed
fn void int_to_ulong(InInt x) { ulong y = x; } // #error: explicit cast

fn void same_signedness_and_widening(InInt i, InUint u, InShort s, CsInt c)
{
	int a = i;
	long b = i;
	uint d = u;
	ulong e = u;
	long f = u;
	short g = s;
	int h = c;
	long k = c;
}

fn void explicit_casts(InInt i, InUint u)
{
	uint a = (uint)i;
	int b = (int)u;
}
