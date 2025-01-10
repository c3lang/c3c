module ringbuffer_test @test;
import std::collections::ringbuffer;
import std::io;

def Buffer = RingBuffer(<char, 4>);

fn void push_get()
{
	Buffer rb;
	rb.init();
	rb.push(1);
	rb.push(2);
	rb.push(3);
	rb.push(4);

	assert(rb.get(0) == 1);
	assert(rb.get(1) == 2);
	assert(rb.get(2) == 3);
	assert(rb.get(3) == 4);

	rb.push(5);
	assert(rb.get(0) == 2);
	assert(rb.get(1) == 3);
	assert(rb.get(2) == 4);
	assert(rb.get(3) == 5);

	char c = rb.pop()!!;
	assert(c == 5);
}