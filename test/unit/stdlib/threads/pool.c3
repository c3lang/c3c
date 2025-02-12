module thread_pool_test;
import std::io;
import std::thread;
import std::thread::pool;

def Pool = ThreadPool(<4>);

fn void init_destroy() @test
{
	for (usz i = 0; i < 20; i++)
	{
		Pool pool;
		pool.init()!!;
		pool.destroy()!!;
	}
}

fn void push_destroy() @test
{
	for FOO: (usz i = 0; i < 20; i++)
	{
		x = 0;
		int y = 20;
		Pool pool;
		pool.init()!!;
		defer pool.destroy()!!;
		work_done.lock()!!;
		pool.push(&do_work, &y)!!;
		work_done.unlock()!!;
		for (int j = 0; j < 1000; j++)
		{
			work_done.lock()!!;
			if (@atomic_load(x) == @atomic_load(y))
			{
				(void)work_done.unlock();
				break FOO;
			}
			(void)work_done.unlock();
			thread::yield();
		}
		assert(false, "y never changed");
	}
}

fn void push_stop() @test
{
	for (usz i = 0; i < 20; i++)
	{
		x = 0;
		int y = 20;
		Pool pool;
		pool.init()!!;
		work_done.lock()!!;
		pool.push(&do_work, &y)!!;
		work_done.unlock()!!;
		pool.stop_and_destroy()!!;
		assert(x == y, "%d: %d != %d", i, x, y);
	}
}

int x;

Mutex work_done;

fn void startup() @init {
	work_done.init()!!;
}

fn void shutdown() @finalizer {
	(void)work_done.destroy();
}

fn int do_work(void* arg)
{
	work_done.lock()!!;
	x = *(int*)arg;
	work_done.unlock()!!;
	return 0;
}