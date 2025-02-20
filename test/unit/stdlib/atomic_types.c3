import std::thread;
import std::io;
import std::atomic::types;

def AtomicUint = Atomic(<uint>);
def AtomicFloat = Atomic(<float>);
AtomicUint a;
AtomicFloat fa;

fn void add() @test
{
	Thread[100] ts;
	a.store(0);
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
			thread::sleep_ms(5);
			a.add(5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a.load() == ts.len * 10 * 5, "Threads returned %d, expected %d", a.load(), ts.len * 10 * 5);
}

fn void sub() @test
{
	Thread[100] ts;
	a.store(ts.len * 10 * 5);
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
			thread::sleep_ms(5);
			a.sub(5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a.load() == 0, "Threads returned %d, expected %d", a.load(), 0);
}

fn void fadd() @test
{
	Thread[100] ts;
	fa.store(0);
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
			thread::sleep_ms(5);
			fa.add(0.5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(fa.load() == ts.len * 10 * 0.5, "Threads returned %f, expected %f", fa.load(), ts.len * 10 * 0.5);
}

fn void fsub() @test
{
	Thread[100] ts;
	fa.store(ts.len * 10 * 0.5);
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
			thread::sleep_ms(5);
			fa.sub(0.5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(fa.load() == 0, "Threads returned %f, expected %f", fa.load(), 0);
}