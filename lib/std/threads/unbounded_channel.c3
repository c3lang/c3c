module std::thread::channel <Type>;
import std::collections::deque;

typedef UnboundedChannel = void;

struct UnboundedChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	bool closed;

	sz read_waiting;
	ConditionVariable read_cond;

	Deque {Type} queue;
}


<*
 @require start_size > 0: "channel size must be > 0"
*>
fn UnboundedChannel*? create_unbounded(Allocator allocator, sz start_size = 16)
{
	UnboundedChannelImpl* channel = alloc::new(allocator, UnboundedChannelImpl);
	defer catch alloc::free(allocator, channel);
	channel.queue.init(allocator, start_size);
	defer catch channel.queue.free();

	channel.allocator = allocator;

	channel.mu.init()!;
	defer catch channel.mu.destroy();
	channel.read_cond.init()!;
	defer catch channel.read_cond.destroy();

	return (UnboundedChannel*)channel;
}

fn void UnboundedChannel.destroy(&self)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;

	channel.mu.destroy();
	channel.read_cond.destroy();
	channel.queue.free();
	alloc::free(channel.allocator, channel);
}

fn void? UnboundedChannel.push(&self, Type val)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;

	channel.mu.lock();
	defer channel.mu.unlock();

	// check if channel is closed
	if (channel.closed) return thread::CHANNEL_CLOSED~;

	// save value to buf
	channel.queue.push_front(val);

	// if someone is waiting -> awake him
	if (channel.read_waiting > 0)
	{
		channel.read_cond.signal();
	}
}

fn Type? UnboundedChannel.try_pop(&self)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;
	channel.mu.lock();
	defer channel.mu.unlock();
	if (channel.queue.is_empty() && !channel.closed) return thread::CHANNEL_EMPTY~;

	return _unbounded_channel_pop_in_lock(channel);
}

fn void UnboundedChannel.clear(&self)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;
	channel.mu.lock();
	defer channel.mu.unlock();
	channel.queue.clear();
}

fn Type? UnboundedChannel.pop(&self)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;
	channel.mu.lock();
	defer channel.mu.unlock();

	// if chan is empty -> wait for sender
	while (channel.queue.is_empty() && !channel.closed)
	{
		channel.read_waiting++;
		channel.read_cond.wait(&channel.mu);
		channel.read_waiting--;
	}
	return _unbounded_channel_pop_in_lock(channel);
}


fn void UnboundedChannel.close(&self)
{
	UnboundedChannelImpl* channel = (UnboundedChannelImpl*)self;

	channel.mu.lock();

	channel.closed = true;

	channel.read_cond.broadcast();
	channel.mu.unlock();

}

fn Type? _unbounded_channel_pop_in_lock(UnboundedChannelImpl* channel) @inline @private
{
	// check if chan is closed and empty
	if (channel.closed && channel.queue.is_empty())
	{
		return thread::CHANNEL_CLOSED~;
	}

	// read from buf
	return channel.queue.pop()!!;

}