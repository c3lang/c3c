module ringbuffer_test @test;
import std::collections::ringbuffer;
import std::io;

def Buffer = RingBuffer(<char, 4>);

fn void! putc_getc()
{
	Buffer rb;
	rb.init();
	rb.putc(1);
	rb.putc(2);
	rb.putc(3);
	rb.putc(4);

	assert(rb.getc(0) == 1);
	assert(rb.getc(1) == 2);
	assert(rb.getc(2) == 3);
	assert(rb.getc(3) == 4);

	rb.putc(5);
	assert(rb.getc(0) == 2);
	assert(rb.getc(1) == 3);
	assert(rb.getc(2) == 4);
	assert(rb.getc(3) == 5);

	char c = rb.popc()!;
	assert(c == 5);
}