module thread_test;

import std::thread::channel;
import std::thread;
import std::time;
import std::io;

fn void init_destroy_buffered() @test
{
	for (sz i = 0; i < 20; i++)
	{
		BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
		defer c.destroy();
	}
}

fn void init_destroy_unbuffered() @test
{
	for (sz i = 0; i < 20; i++)
	{
		UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
		defer c.destroy();
	}
}

fn void init_destroy_oneshot() @test
{
	for (usz i = 0; i < 20; i++)
	{
		OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
		defer c.destroy();
	}
}

fn void push_to_buffered_channel_no_lock() @test
{
	BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
	defer c.destroy();

	c.push(1)!!;
}

fn void push_pop_buffered_no_locks() @test
{
	BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
	defer c.destroy();


	c.push(123)!!;
	int got = c.pop()!!;
	assert(got == 123);
}


fn void test_unbuffered_channel_race() @test
{
	UnbufferedChannel{int}* ch = channel::create_unbuffered(mem)!!;
	defer ch.destroy();

	Thread t1, t2;

	// Sender 1
	t1.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* ch = (UnbufferedChannel*)arg;

		for (int i = 0; i < 100000; i++)
		{
			ch.push(1)!!;
		}
		return 0;
	}, ch)!!;

	// Sender 2
	t2.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* ch = (UnbufferedChannel*)arg;

		for (int i = 0; i < 100000; i++)
		{
			ch.push(2)!!;
		}
		return 0;
	}, ch)!!;

	int count1 = 0;
	int count2 = 0;

	// Receiver
	for (int i = 0; i < 200000; i++)
	{
		int v = ch.pop()!!;
		switch (v)
		{
			case 1: count1++;
			case 2: count2++;
			default: unreachable();
		}
	}

	t1.join();
	t2.join();

	assert(count1 == count2);

}


fn void test_buffered_channel_race() @test
{
	BufferedChannel{int}* ch = channel::create_buffered(mem, 1)!!;
	defer ch.destroy();

	Thread t1, t2;

	// Sender 1
	t1.create(fn int(void* arg)
	{
		BufferedChannel{int}* ch = (BufferedChannel*)arg;

		for (int i = 0; i < 100000; i++)
		{
			ch.push(1)!!;
		}
		return 0;
	}, ch)!!;

	// Sender 2
	t2.create(fn int(void* arg)
	{
		BufferedChannel{int}* ch = (BufferedChannel*)arg;

		for (int i = 0; i < 100000; i++)
		{
			ch.push(2)!!;
		}
		return 0;
	}, ch)!!;

	int count1 = 0;
	int count2 = 0;

	// Receiver
	for (int i = 0; i < 200000; i++)
	{
		int v = ch.pop()!!;
		switch (v)
		{
			case 1: count1++;
			case 2: count2++;
			default: unreachable();
		}
	}

	t1.join();
	t2.join();

	assert(count1 == count2);
	assert(count1 + count2 == 200000);

}

fn void push_pop_unbuffered_with_locks() @test
{
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* c = (UnbufferedChannel*)arg;
		c.push(123)!!;
		c.push(321)!!;
		return 0;
	}, (void*)c)!!;

	int got = c.pop()!!;
	assert(got == 123);
	got = c.pop()!!;
	assert(got == 321);
}

fn void push_pop_oneshot_no_locks() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	c.push(123)!!;
	int got = c.pop()!!;
	assert(got == 123);
}

fn void push_pop_oneshot_with_locks() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		OneShotChannel{int}* c = (OneShotChannel*)arg;
		c.push(123)!!;
		return 0;
	}, (void*)c)!!;

	int got = c.pop()!!;
	assert(got == 123);
}

fn void sending_to_closed_unbuffered_chan_is_forbidden() @test
{
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
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
	BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
	defer c.destroy();


	c.close();

	if (catch err = c.push(123))
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void sending_twice_to_oneshot_chan_is_forbidden() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	c.push(123)!!;

	if (catch err = c.push(321))
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void reading_from_empty_closed_unbuffered_chan_is_forbidden() @test
{
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
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
	BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
	defer c.destroy();


	c.close();

	if (catch err = c.pop())
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}
	assert(false);
}

fn void reading_from_empty_closed_oneshot_chan_is_forbidden() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
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
	BufferedChannel{int}* c = channel::create_buffered(mem, 3)!!;
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

fn void reading_from_sent_closed_oneshot_chan_is_ok() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	c.push(123)!!;
	c.close();

	int got = c.pop()!!;
	assert(got == 123);

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
	BufferedChannel{int}* c = channel::create_buffered(mem, 3)!!;
	defer c.destroy();


	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int}* c = (BufferedChannel*)arg;
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
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* c = (UnbufferedChannel*)arg;
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

fn void reading_from_oneshot_chan_aborted_by_close() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		OneShotChannel{int}* c = (OneShotChannel*)arg;
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
	BufferedChannel{int}* c = channel::create_buffered(mem, 1)!!;
	defer c.destroy();


	c.push(1)!!;

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int}* c = (BufferedChannel*)arg;
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
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
	defer c.destroy();

	Thread thread;

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* c = (UnbufferedChannel*)arg;
		c.close();
		return 0;
	}, (void*)c)!!;

	thread.join();
	fault err = @catch(c.push(1));
	if (err)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void reading_from_consumed_oneshot_chan_is_forbidden() @test
{
	OneShotChannel{int}* c = channel::create_one_shot(mem)!!;
	defer c.destroy();

	c.push(123)!!;
	assert(c.pop()!! == 123);

	int? res = c.pop();
	if (catch err = res)
	{
		assert(err == thread::CHANNEL_CLOSED);
		return;
	}

	assert(false);
}

fn void multiple_actions_unbuffered() @test
{
	UnbufferedChannel{int}* c = channel::create_unbuffered(mem)!!;
	defer c.destroy();

	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		UnbufferedChannel{int}* c = (UnbufferedChannel*)arg;
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
	BufferedChannel{int}* c = channel::create_buffered(mem, 10)!!;
	defer c.destroy();


	Thread thread;
	defer thread.join();

	thread.create(fn int(void* arg)
	{
		BufferedChannel{int}* c = (BufferedChannel*)arg;
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
