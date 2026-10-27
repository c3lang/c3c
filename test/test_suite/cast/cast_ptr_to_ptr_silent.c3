module test;

alias Vec4 = int[<4>] @simd;
typedef MyInt = int;

// A silent probe ($defined) must give the same answer as the real conversion, and print nothing.
// Each check is in its own function so that one failure doesn't hide the others.
fn void distinct_pointee_is_not_implicit()
{
	int x;
	int* p = &x;
	$assert(!$defined(MyInt* q = p));
}

fn void alignment_increase_is_not_implicit()
{
	int[4]* p;
	$assert(!$defined(Vec4* q = p));
}

// The real conversions are errors.
fn void distinct_pointee_loud()
{
	int x;
	int* p = &x;
	MyInt* q = p; // #error: Implicitly casting
}

fn void alignment_increase_loud()
{
	int[4]* p;
	Vec4* q = p; // #error: alignment
}

// These already work, and must keep working.
fn void same_type()
{
	int x;
	int* p = &x;
	$assert($defined(int* q = p));
}

fn void to_void_ptr()
{
	int x;
	int* p = &x;
	$assert($defined(void* q = p));
}

fn void from_void_ptr()
{
	void* v;
	$assert($defined(int* q = v));
}

fn void explicit_cast_to_distinct_pointee()
{
	int x;
	int* p = &x;
	$assert($defined(MyInt* q = (MyInt*)p));
}

fn void alignment_decrease()
{
	Vec4* p;
	$assert($defined(int[4]* q = p));
}
