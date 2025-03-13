import std::thread;
import std::io;
import std::atomic::types;

alias AtomicUint = Atomic{uint};
alias AtomicFloat = Atomic{float};
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

fn void fops() @test
{
	fa.store(100);
	fa.mul(2);
	fa.div(3.4);
}

fn void iops() @test
{
	a.store(100);
	a.mul(3);
	a.div(5);
	a.shr(1);
	a.shl(3);
	a.xor(4);
	a.or(5);
	a.and(6);
}

fn void bops() @test
{
	Atomic {bool} ab;
	ab.store(true);
	ab.store(false);
	ab.xor(true);
	ab.or(false);
	ab.store(false);
	test::eq(false, ab.set());
	test::eq(true, ab.clear());
	test::eq(false, ab.clear());
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