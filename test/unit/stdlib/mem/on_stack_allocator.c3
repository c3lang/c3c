module on_stack_allocator_test @test;
import std::core::mem;

// Regression test for: OnStackAllocator.acquire() ignores init_type on the
// stack-buffer path, returning unzeroed memory even when the caller used
// calloc / requested ZERO initialisation.
fn void test_on_stack_allocator_calloc_zeroes() @test
{
    // Pre-fill the backing buffer with a non-zero sentinel so we can detect
    // whether the allocation was actually zeroed.
    char[256] buf;
    mem::set(&buf, 0xAA, 256);

    OnStackAllocator stack;
    stack.init(&buf, mem);
    defer stack.free();

    // calloc must return zeroed memory even when served from the stack buffer.
    char* p = alloc::calloc(&stack, 16);

    for (int i = 0; i < 16; i++)
    {
        assert(p[i] == 0, "calloc from stack buffer did not zero byte %d", i);
    }
}
