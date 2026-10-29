module test;

typedef Idx = inline int;
typedef UIdx = inline ulong;
typedef SmallIdx = inline char;

enum Foo { A, B, C }

int[3] data = { 10, 20, 30 };

fn int? get() => 1;

// An optional index is fine when its underlying type is an integer, also through an inline distinct
// type. Each check is in its own function so that one failure doesn't hide the others.
fn void optional_distinct_index()
{
	Idx? k = (Idx)1;
	int c = data[k]!!;
}

fn void optional_unsigned_distinct_index()
{
	UIdx? k = (UIdx)1;
	int c = data[k]!!;
}

fn void optional_small_distinct_index()
{
	SmallIdx? k = (SmallIdx)1;
	int c = data[k]!!;
}

// The result of indexing with an optional index is still optional.
fn void result_stays_optional()
{
	Idx? k = (Idx)1;
	int? r = data[k];
}

fn void optional_distinct_range_start()
{
	Idx? k = (Idx)1;
	int[] r = data[k..2]!!;
}

fn void optional_distinct_range_length()
{
	Idx? k = (Idx)2;
	int[] r = data[0:k]!!;
}

// These already work, and must keep working.
fn void plain_distinct_index()
{
	Idx k = (Idx)1;
	int c = data[k];
}

fn void optional_int_index()
{
	int? j = get();
	int c = data[j]!!;
}

fn void optional_enum_index()
{
	Foo? e = B;
	int c = data[e]!!;
}

fn void plain_distinct_range()
{
	Idx k = (Idx)1;
	int[] r = data[k..2];
}

// A distinct type that is not inline is not an integer index, optional or not.
typedef Opaque = int;

fn void opaque_index()
{
	Opaque k = (Opaque)1;
	int c = data[k]; // #error: An integer value was expected here
}

fn void optional_opaque_index()
{
	Opaque? k = (Opaque)1;
	int c = data[k]!!; // #error: An integer value was expected here
}
