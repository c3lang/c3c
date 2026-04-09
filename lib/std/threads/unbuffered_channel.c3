module std::thread::channel <Type>;

typedef UnbufferedChannel = void;

struct UnbufferedChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	Type val;
	bool closed;
	bool has_value;
	sz send_waiting;
	ConditionVariable send_cond;

	sz read_waiting;
	ConditionVariable read_cond;
}

fn UnbufferedChannel*? create_unbuffered(Allocator allocator)
{
	UnbufferedChannelImpl* channel = alloc::alloc(allocator, UnbufferedChannelImpl);
	defer catch (void)alloc::free(allocator, channel);

	channel.allocator = allocator;
	channel.send_waiting = 0;
	channel.read_waiting = 0;

	channel.mu.init()!;
	defer catch channel.mu.destroy();
	channel.send_cond.init()!;
	defer catch channel.send_cond.destroy();
	channel.read_cond.init()!;

	return (UnbufferedChannel*)channel;
}

fn void UnbufferedChannel.destroy(&self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)(self);
	channel.mu.destroy();
	channel.send_cond.destroy();
	channel.read_cond.destroy();
	alloc::free(channel.allocator, channel);
}

fn void? UnbufferedChannel.push(&self, Type val)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	channel.mu.lock();
	defer channel.mu.unlock();

	// if channel is full -> wait
	while (channel.has_value && !channel.closed)
	{
		channel.send_waiting++;
		channel.send_cond.wait(&channel.mu);
		channel.send_waiting--;
	}

	// check if channel is closed
	if (channel.closed) return thread::CHANNEL_CLOSED~;

	// save value to buf
	channel.val = val;
	channel.has_value = true;

	// if someone is waiting -> awake him
	if (channel.read_waiting > 0)
	{
		channel.read_cond.signal();
	}
}

fn Type? UnbufferedChannel.pop(&self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	channel.mu.lock();
	defer channel.mu.unlock();

	// if chan is empty -> wait for sender
	while (!channel.has_value && !channel.closed)
	{
		channel.read_waiting++;
		channel.read_cond.wait(&channel.mu);
		channel.read_waiting--;
	}

	// check if chan is closed and empty
	if (channel.closed && !channel.has_value)
	{
		return thread::CHANNEL_CLOSED~;
	}

	// read from buf
	Type ret = channel.val;

	channel.has_value = false;

	// if someone is waiting -> awake him
	if (channel.send_waiting > 0)
	{
		channel.send_cond.signal();
	}

	return ret;
}

fn void UnbufferedChannel.close(&self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	channel.mu.lock();

	channel.closed = true;

	channel.read_cond.broadcast();
	channel.send_cond.broadcast();
	channel.mu.unlock();

}