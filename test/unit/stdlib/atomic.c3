import std::thread;
import std::io;
import std::atomic;

uint a;

fn void! add() @test
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
    }, null)!;
	}
	foreach (&t : ts)
  {
    assert(t.join()! == 0);
  }
	assert(a == ts.len * 10 * 5, "Threads returned %d, expected %d", a, ts.len * 10 * 5);
}

fn void! sub() @test
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
    }, null)!;
	}
	foreach (&t : ts)
  {
    assert(t.join()! == 0);
  }
	assert(a == 0, "Threads returned %d, expected %d", a, 0);
}
