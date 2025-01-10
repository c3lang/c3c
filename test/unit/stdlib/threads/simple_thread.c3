import std::thread;
import std::io;

int a;

fn void testrun() @test
{
	Thread t;
	a = 0;
	t.create(fn int(void* arg) { a++; return 0; }, null)!!;
	assert(t.join()!! == 0);
	assert(a == 1);

	t.create(fn int(void* arg) { return 10; }, null)!!;
	assert(t.join()!! == 10);
}


Mutex m_global;

fn void testrun_mutex() @test
{
	Thread[20] ts;
	a = 0;
	m_global.init()!!;
	foreach (&t : ts)
	{
		t.create(fn int(void* arg) {
			m_global.lock()!!;
			defer m_global.unlock()!!;
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
		}, null)!!;
	}
	foreach (&t : ts)
    {
        assert(t.join()!! == 0);
    }
	assert(a == ts.len);
	m_global.destroy()!!;
}

fn void testrun_mutex_try() @test
{
	Mutex m;
	m.init()!!;
	m.lock()!!;
	assert(m.try_lock() == false);
	m.unlock()!!;
	assert(m.try_lock() == true);
	m.unlock()!!;
}

fn void testrun_mutex_timeout() @test
{
	TimedMutex m;
	m.init()!!;
	m.lock()!!;
	if (try m.lock_timeout(20))
	{
		unreachable("lock_timeout should fail");
	}
	m.unlock()!!;
	m.lock_timeout(20)!!;
	m.unlock()!!;
}

int x_once = 100;

fn void call_once()
{
	x_once += 100;
}

fn void testrun_once() @test
{
	OnceFlag once;
	once.call(&call_once);
	assert(x_once == 200);
	once.call(&call_once);
	assert(x_once == 200);
}