// #target: macos-x64
module test;

// An integer that is not exactly pointer sized cannot be cast to a pointer. A constant is still
// checked against the range of a pointer instead.
// The matched text is just "pointer size", which the existing message for smaller integers contains.
// Each check is in its own function so that one failure doesn't hide the others.
fn void int128_to_void_ptr()
{
	int128 big = 5;
	void* p = (void*)big; // #error: pointer size
}

fn void uint128_to_void_ptr()
{
	uint128 big = 5;
	void* p = (void*)big; // #error: pointer size
}

fn void int128_to_typed_ptr()
{
	int128 big = 5;
	int* p = (int*)big; // #error: pointer size
}

// These already work, and must keep working.
fn void smaller_than_pointer()
{
	int v = 5;
	void* p = (void*)v; // #error: smaller than pointer size
}

fn void uptr_to_ptr()
{
	uptr v = 5;
	void* p = (void*)v;
}

fn void iptr_to_ptr()
{
	iptr v = 5;
	void* p = (void*)v;
}

fn void constant_that_fits()
{
	void* p = (void*)(int128)5;
}

fn void constant_that_does_not_fit()
{
	void* p = (void*)(uint128)0xFFFF_FFFF_FFFF_FFFF_FFFF; // #error: does not fit in a pointer
}
