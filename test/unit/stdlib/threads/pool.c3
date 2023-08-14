module thread_pool_test;
import std::io;
import std::thread;
import std::thread::pool;

def Pool = ThreadPool(<4>);

fn void! init_destroy() @test
{
	for (usz i = 0; i < 20; i++)
	{
		Pool pool;
		pool.init()!;
		pool.destroy()!;
	}
}

fn void! push_destroy() @test
{
	for (usz i = 0; i < 20; i++)
	{
		x = 0;
		int y = 20;
		Pool pool;
		pool.init()!;
		defer pool.destroy()!!;
		work_done.lock()!!;
		pool.push(&do_work, &y)!;
		work_done.lock()!!;
		assert(x == y, "%d: %d != %d", i, x, y);
		work_done.unlock()!!;
	}
}

fn void! push_stop() @test
{
	for (usz i = 0; i < 20; i++)
	{
		x = 0;
		int y = 20;
		Pool pool;
		pool.init()!;
		work_done.lock()!!;
		pool.push(&do_work, &y)!;
		pool.stop_and_destroy()!!;
		assert(x == y, "%d: %d != %d", i, x, y);
	}
}

int x;

Mutex work_done;

static initialize {
	work_done.init()!!;
}

static finalize {
	work_done.destroy()!!;
}

fn int do_work(void* arg)
{
	x = *(int*)arg;
	work_done.unlock()!!;
	return 0;
}