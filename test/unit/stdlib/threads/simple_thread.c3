import std::thread;
import std::io;
int a;
fn void! testrun() @test
{
	Thread t;
	a = 0;
	t.create(fn int(void* arg) { a++; return 0; }, null);
	assert(t.join()! == 0);
	assert(a == 1);
}

Mutex m;

fn void! testrun_mutex() @test
{
	Thread[100] ts;
	a = 0;
	m.init(thread::MUTEX_PLAIN);
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			m.lock();
			defer m.unlock()!!;
			a += 10;
			thread::sleep_ms(5);
			a *= 10;
			thread::sleep_ms(5);
			a /= 10;
			thread::sleep_ms(5);
			a -= 10;
			thread::sleep_ms(5);
			a++;
		    return 0;
		}, null);
	}
	foreach (i, &t : ts)
    {
        assert(t.join()! == 0);
    }
	assert(a == 100);
}