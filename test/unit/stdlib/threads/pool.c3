module thread_pool_test;
import std::io, std::thread, std::time;

alias Pool = ThreadPool{4};

fn void init_destroy() @test
{
	for (sz i = 0; i < 20; i++)
	{
		Pool pool;
		pool.init()!!;
		pool.destroy();
	}
}

int x1;

fn void push_destroy() @test
{
	for FOO: (sz i = 0; i < 20; i++)
	{
		@atomic_store(x1, 0);
		int y = 20;
		Pool pool;
		pool.init()!!;
		defer pool.destroy();
		pool.push(&do_work1, &y);
		for (sz j = 0; j < 500; j++)
		{
			if (@atomic_load(x1) == @atomic_load(y)) break;
			thread::sleep(time::ms(1));
		}
		test::eq(@atomic_load(x1), @atomic_load(y));
	}
}
fn int do_work1(void* arg)
{
	@atomic_store(x1, @atomic_load(*(int*)arg));
	return 0;
}

int x2;

fn void push_stop() @test
{
	for (sz i = 0; i < 20; i++)
	{
		@atomic_store(x2, 0);
		int y = 20;
		Pool pool;
		pool.init()!!;
		pool.push(&do_work2, &y);
		pool.stop_and_destroy();
		test::eq(@atomic_load(x2), @atomic_load(y));
	}
}

fn int do_work2(void* arg)
{
	@atomic_store(x2, @atomic_load(*(int*)arg));
	return 0;
}


int x3;
fn void join() @test
{
	@atomic_store(x3, 0);
	Pool pool;
	pool.init()!!;
	defer pool.stop_and_destroy();
	for (sz i = 0; i < 4; i++)
	{
		pool.push(&do_wait, (void*)i);
	}
	pool.join();
	test::eq(x3, 6);
}

fn int do_wait(void* arg)
{
	sz value = (sz)(iptr)arg;
	for (sz i = 0; i < value; i++) thread::sleep(time::ms(10));
	@atomic_store(x3, @atomic_load(x3) + (int)value);
	return 0;
}