module test;

enum Foo { A, B }
typedef Aaa = int;
typedef Ccc = float;

// An optional value gets the same helpful hint as the plain value.
// Each check is in its own function so that one failure doesn't hide the others.
// The enum hint is matched on "ordinal"; if its wording changes (see cast_enum_to_int_message.c3), keep
// the word, or change both tests.

fn void enum_plain()
{
	Foo e = A;
	int y = e; // #error: ordinal
}

fn void enum_optional()
{
	Foo? e = A;
	int y = e; // #error: ordinal
}

fn void distinct_plain()
{
	Aaa a = (Aaa)1;
	Ccc c = a; // #error: usually not intended
}

fn void distinct_optional()
{
	Aaa? a = (Aaa)1;
	Ccc c = a; // #error: usually not intended
}

fn void zstring_plain()
{
	ZString z = "abc";
	String s = z; // #error: str_view
}

fn void zstring_optional()
{
	ZString? z = "abc";
	String s = z; // #error: str_view
}

fn void pointer_to_slice_plain()
{
	int x;
	int* p = &x;
	int[] s = p; // #error: slice the pointer
}

fn void pointer_to_slice_optional()
{
	int x;
	int*? p = &x;
	int[] s = p; // #error: slice the pointer
}
