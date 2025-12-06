module thread_pool_test;
import std::io, std::thread, std::time;

alias Pool = ThreadPool{4};

fn void init_destroy() @test
{
	for (usz i = 0; i < 20; i++)
	{
		Pool pool;
		pool.init()!!;
		pool.destroy();
	}
}

fn void push_destroy() @test
{
	for FOO: (usz i = 0; i < 20; i++)
	{
		@atomic_store(x, 0);
		int y = 20;
		Pool pool;
		pool.init()!!;
		defer pool.destroy();
		pool.push(&do_work, &y);
		thread::sleep(time::ms(50));
		test::eq(@atomic_load(x), @atomic_load(y));
	}
}

fn void push_stop() @test
{
	for (usz i = 0; i < 20; i++)
	{
		@atomic_store(x, 0);
		int y = 20;
		Pool pool;
		pool.init()!!;
		pool.push(&do_work, &y);
		pool.stop_and_destroy();
		test::eq(@atomic_load(x), @atomic_load(y));
	}
}

fn void join() @test
{
	@atomic_store(x, 0);
	Pool pool;
	pool.init()!!;
	defer pool.stop_and_destroy();
	for (usz i = 0; i < 4; i++)
	{
		pool.push(&do_wait, (void*)i);
	}
	pool.join();
	test::eq(x, 6);
}

int x;

fn int do_work(void* arg)
{
	@atomic_store(x, @atomic_load(*(int*)arg));
	return 0;
}

fn int do_wait(void* arg)
{
	usz value = (iptr)arg;
	for (usz i = 0; i < value; i++) thread::sleep(time::ms(50));
	@atomic_store(x, @atomic_load(x) + (int)value);
	return 0;
}