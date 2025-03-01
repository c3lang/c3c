import std::thread;
import std::io;
import std::atomic;

uint a;
float fa;

fn void add() @test
{
	Thread[100] ts;
	a = 0;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_add(&a, 5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a == ts.len * 10 * 5, "Threads returned %d, expected %d", a, ts.len * 10 * 5);
}

fn void sub() @test
{
	Thread[100] ts;
	a = ts.len * 10 * 5;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
			thread::sleep_ms(5);
			atomic::fetch_sub(&a, 5);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a == 0, "Threads returned %d, expected %d", a, 0);
}

fn void div() @test
{
	Thread[8] ts;
	a = 8 * 8 * 8 * 8 * 8 * 8 * 8 * 8 * 8;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			atomic::fetch_div(&a, 8);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a == 8, "Threads returned %d, expected %d", a, 8);
}

fn void max() @test
{
	Thread[100] ts;
	a = 0;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			uint la = 0;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
			thread::sleep_ms(5);
			atomic::fetch_max(&a, la);
			la++;
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a == 5, "Threads returned %d, expected %d", a, 5);
}

fn void min() @test
{
	Thread[100] ts;
	a = 10;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			uint la = 5;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
			thread::sleep_ms(5);
			atomic::fetch_min(&a, la);
			la--;
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(a == 0, "Threads returned %d, expected %d", a, 0);
}

fn void fadd() @test
{
	Thread[100] ts;
	fa = 0;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_add(&fa, 0.5f);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(fa == ts.len * 10 * 0.5, "Threads returned %f, expected %f", fa, ts.len * 10 * 0.5);
}

fn void fsub() @test
{
	Thread[100] ts;
	fa = ts.len * 10 * 0.5;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
			thread::sleep_ms(5);
			atomic::fetch_sub(&fa, 0.5f);
				return 0;
		}, null)!!;
	}
	foreach (&t : ts)
	{
		assert(t.join()!! == 0);
	}
	assert(fa == 0, "Threads returned %f, expected %f", fa, 0);
}