module std::thread::channel <Type>;

typedef BufferedChannel = void;

struct BufferedChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	bool closed;
	sz size;
	sz elems;

	sz sendx;
	sz send_waiting;
	ConditionVariable send_cond;

	sz readx;
	sz read_waiting;
	ConditionVariable read_cond;

	Type[*] buf;
}


<*
 @require size > 0: "channel size must be > 0"
*>
fn BufferedChannel*? create_buffered(Allocator allocator, sz size = 1)
{
	BufferedChannelImpl* channel = alloc::new_with_padding(allocator, BufferedChannelImpl, Type::size * size)!;
	defer catch alloc::free(allocator, channel);

	channel.allocator = allocator;
	channel.size = size;

	channel.mu.init()!;
	defer catch channel.mu.destroy();
	channel.send_cond.init()!;
	defer catch channel.send_cond.destroy();
	channel.read_cond.init()!;
	defer catch channel.read_cond.destroy();

	return (BufferedChannel*)channel;
}

fn void BufferedChannel.destroy(&self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.destroy();
	channel.send_cond.destroy();
	channel.read_cond.destroy();
	alloc::free(channel.allocator, channel);
}

fn void? BufferedChannel.push(&self, Type val)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.lock();
	defer channel.mu.unlock();

	// if channel is full -> wait
	while (channel.elems == channel.size && !channel.closed)
	{
		channel.send_waiting++;
		channel.send_cond.wait(&channel.mu);
		channel.send_waiting--;
	}

	// check if channel is closed
	if (channel.closed) return thread::CHANNEL_CLOSED~;

	// save value to buf
	channel.buf[channel.sendx] = val;

	// move pointer
	channel.sendx++;
	if (channel.sendx == channel.size)
	{
		channel.sendx = 0;
	}

	// channelge elems counter
	channel.elems++;

	// if someone is waiting -> awake him
	if (channel.read_waiting > 0)
	{
		channel.read_cond.signal();
	}
}

fn Type? BufferedChannel.pop(&self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.lock();
	defer channel.mu.unlock();

	// if chan is empty -> wait for sender
	while (channel.elems == 0 && !channel.closed)
	{
		channel.read_waiting++;
		channel.read_cond.wait(&channel.mu);
		channel.read_waiting--;
	}

	// check if chan is closed and empty
	if (channel.closed && channel.elems == 0)
	{
		return thread::CHANNEL_CLOSED~;
	}

	// read from buf
	Type ret = channel.buf[channel.readx];

	// move pointer
	channel.readx++;
	if (channel.readx == channel.size)
	{
		channel.readx = 0;
	}

	// change elems counter
	channel.elems--;

	// if someone is waiting -> awake him
	if (channel.send_waiting > 0)
	{
		channel.send_cond.signal();
	}

	return ret;
}

fn void BufferedChannel.close(&self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.lock();

	channel.closed = true;

	channel.read_cond.broadcast();
	channel.send_cond.broadcast();
	channel.mu.unlock();

}
