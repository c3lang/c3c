module thread_test;

import std::thread::channel;
import std::thread;
import std::time;
import std::io;

fn void init_destroy_buffered() @test
{
	for (usz i = 0; i < 20; i++)
	{
		BufferedChannel{int} c;
		c.init(mem, 1)!!;
		defer c.destroy();
	}
}

fn void init_destroy_unbuffered() @test
{
	for (usz i = 0; i < 20; i++)
	{
		UnbufferedChannel{int} c;
		c.init(mem)!!;
		defer c.destroy();
	}
}

fn void push_to_buffered_channel_no_lock() @test
{
	BufferedChannel{int} c;
	c.init(mem, 1)!!;
	defer c.destroy();

	c.push(1)!!;
}

fn void push_pop_buffered_no_locks() @test
{
	BufferedChannel{int} c;
	c.init(mem, 1)!!;
	defer c.destroy();

	c.push(123)!!;
	int got = c.pop()!!;
	assert(got == 123);
}

fn void push_pop_unbuffered_with_locks() @test
{
	UnbufferedChannel{int} c;
	c.init(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int} c = (UnbufferedChannel{int})arg;
		c.push(123)!!;
		c.push(321)!!;
		return 0;
	}, (void*)c)!!;

	int got = c.pop()!!;
	assert(got == 123);
	got = c.pop()!!;
	assert(got == 321);
}

fn void sending_to_closed_unbuffered_chan_is_forbidden() @test
{
	UnbufferedChannel{int} c;
	c.init(mem, )!!;
	defer c.destroy();

	c.close();

	if (catch err = c.push(123))
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void sending_to_closed_buffered_chan_is_forbidden() @test
{
	BufferedChannel{int} c;
	c.init(mem, 1)!!;
	defer c.destroy();

	c.close();

	if (catch err = c.push(123))
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void reading_from_empty_closed_unbuffered_chan_is_forbidden() @test
{
	UnbufferedChannel{int} c;
	c.init(mem, )!!;
	defer c.destroy();

	c.close();

	if (catch err = c.pop())
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void reading_from_empty_closed_buffered_chan_is_forbidden() @test
{
	BufferedChannel{int} c;
	c.init(mem, 1)!!;
	defer c.destroy();

	c.close();

	if (catch err = c.pop())
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void reading_from_non_empty_closed_buffered_chan_is_ok() @test
{
	BufferedChannel{int} c;
	c.init(mem, 3)!!;
	defer c.destroy();

	c.push(1)!!;
	c.push(2)!!;
	c.push(3)!!;

	c.close();

	int got = c.pop()!!;
	assert(got == 1);
	got = c.pop()!!;
	assert(got == 2);
	got = c.pop()!!;
	assert(got == 3);

	int? got_err = c.pop();
	if (catch err = got_err)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void reading_from_empty_buffered_chan_aborted_by_close() @test
{
	BufferedChannel{int} c;
	c.init(mem, 3)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int} c = (BufferedChannel{int})arg;
		c.close();
		return 0;
	}, (void*)c)!!;

	int? res = c.pop();
	if (catch err = res)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void reading_from_unbuffered_chan_aborted_by_close() @test
{
	UnbufferedChannel{int} c;
	c.init(mem, )!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int} c = (UnbufferedChannel{int})arg;
		c.close();
		return 0;
	}, (void*)c)!!;

	int? res = c.pop();
	if (catch err = res)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void sending_to_full_buffered_chan_aborted_by_close() @test
{
	BufferedChannel{int} c;
	c.init(mem, 1)!!;
	defer c.destroy();

	c.push(1)!!;

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int} c = (BufferedChannel{int})arg;
		c.close();
		return 0;
	}, (void*)c)!!;

	fault err = @catch(c.push(1));
	if (err)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void sending_to_unbuffered_chan_aborted_by_close() @test
{
	UnbufferedChannel{int} c;
	c.init(mem, )!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int} c = (UnbufferedChannel{int})arg;
		c.close();
		return 0;
	}, (void*)c)!!;

	fault err = @catch(c.push(1));
	if (err)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void multiple_actions_unbuffered() @test
{
	UnbufferedChannel{int} c;
	c.init(mem, )!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int} c = (UnbufferedChannel{int})arg;
		for (int i = 0; i <= 100; i++)
		{
			c.push(i)!!;
		}
		return 0;
	}, (void*)c)!!;

	int sum;

	for (int i = 0; i <= 100; i++)
	{
		int? res = c.pop();
		if (catch err = res)
		{
			assert(false);
		}
		sum += res;
	}

	assert(sum == 5050);
}

fn void multiple_actions_buffered() @test
{
	BufferedChannel{int} c;
	c.init(mem, 10)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int} c = (BufferedChannel{int})arg;
		for (int i = 0; i <= 100; i++)
		{
			c.push(i)!!;
		}
		return 0;
	}, (void*)c)!!;

	int sum;

	for (int i = 0; i <= 100; i++)
	{
		int? res = c.pop();
		if (catch err = res)
		{
			assert(false);
		}
		sum += res;
	}

	assert(sum == 5050);
}

