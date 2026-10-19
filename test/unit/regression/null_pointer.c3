module null_pointer;
import std, libc;

struct Foo
{
	int a;
	long b;
}

fn void test_null_member_offset() @test
{
	Foo* f = null;
	sz offset = (sz)&f.b;
	assert(offset == $reflect(Foo.b).offset);
}

JmpBuf test_buf;
bool panicked = false;

fn void test_panic_handler(String message, String file, String function, int line)
{
	panicked = true;
	libc::longjmp(&test_buf, 1);
}

// On Windows ARM64, libc::setjmp/longjmp integrates with Windows SEH unwinding (RtlUnwindEx),
// which requires the caller function's entry stack pointer (sponentry) to be passed as the
// frame parameter to _setjmpex. Until a compiler intrinsic (e.g. `$$sponentry` / `llvm.sponentry`)
// is implemented to provide this, calling longjmp across frames triggers STATUS_BAD_STACK (0xC0000028).
fn void test_null_deref_panic() @test @feat(!(WIN32 & AARCH64))
{
	PanicFn saved = builtin::panic;
	builtin::panic = &test_panic_handler;
	panicked = false;
	if (libc::setjmp(&test_buf) == 0)
	{
		int* p = null;
		int x = *p;
		(void)x;
	}
	builtin::panic = saved;
	assert(panicked, "dereferencing null pointer should panic");
}
