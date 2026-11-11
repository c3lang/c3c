// Builtins without a standard library macro, checked at runtime on both backends.
module std::core::builtins::codegen_test @test;

fn void reverse_vector()
{
	int[<4>] a = { 1, -3, 5, -7 };
	assert($$reverse(a) == (int[<4>]){ -7, 5, -3, 1 });
	char[<3>] b = { 7, 8, 9 };
	char[<3>] r = $$reverse(b);
	assert(r[0] == 9 && r[1] == 8 && r[2] == 7);
	float[<2>] f = { 0.5f, 2.0f };
	assert($$reverse(f) == (float[<2>]){ 2.0f, 0.5f });
}

// Arithmetic results are not checked: optimizers fold FP operations assuming the default rounding mode.
// LLVM fails to select set_rounding on wasm ("Cannot select: set_rounding").
fn void rounding_mode() @feat(!WASM)
{
	int old = $$get_rounding_mode();
	$$set_rounding_mode(2);
	assert($$get_rounding_mode() == 2);
	$$set_rounding_mode(3);
	assert($$get_rounding_mode() == 3);
	$$set_rounding_mode(0);
	assert($$get_rounding_mode() == 0);
	$$set_rounding_mode(old);
	assert($$get_rounding_mode() == old);
}

// Termux's LLVM fails to select atomicrmw uinc_wrap on AArch64 ("Cannot select: AtomicLoadUIncWrap").
fn void atomic_fetch_wrap() @feat(!ANDROID)
{
	uint x = 4;
	// inc_wrap: old >= v ? 0 : old + 1
	assert($$atomic_fetch_inc_wrap(&x, 5u, false, 5, 0) == 4);
	assert(x == 5);
	assert($$atomic_fetch_inc_wrap(&x, 5u, false, 5, 0) == 5);
	assert(x == 0);
	// dec_wrap: old == 0 || old > v ? v : old - 1
	assert($$atomic_fetch_dec_wrap(&x, 4u, false, 5, 0) == 0);
	assert(x == 4);
	assert($$atomic_fetch_dec_wrap(&x, 4u, false, 5, 0) == 4);
	assert(x == 3);
	ulong y = 9;
	assert($$atomic_fetch_dec_wrap(&y, 4ul, false, 5, 0) == 9);
	assert(y == 4);
}

fn void wasm_memory_outside_wasm() @feat(!WASM)
{
	assert($$wasm_memory_size(0) == 0);
	assert($$wasm_memory_grow(0, 1) == -1);
}
