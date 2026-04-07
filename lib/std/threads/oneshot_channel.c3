module std::thread::channel <Type>;

typedef OneShotChannel = void;

struct OneShotChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	ConditionVariable cond;
	Type value;
	bool sent;
	bool consumed;
	bool closed;
}

<*
 Initialize a one-shot channel for sending exactly one value between threads.

 The first successful `push` stores the value, and the first successful `pop`
 receives it. Closing the channel wakes blocked receivers; if a value was
 already sent before closing, it remains receivable once.

 Example:
 ```c3
 OneShotChannel{int}* channel = channel::new_one_shot(mem);
 defer channel.destroy();

 Thread thread;
 thread.create(fn int(void* arg)
 {
 	OneShotChannel{int}* channel = arg;
 	channel.push(123)!!;
 	return 0;
 }, (void*)channel)!!;
 defer thread.join();

 int value = channel.pop()!!;
 ```
*>
fn OneShotChannel*? create_one_shot(Allocator allocator)
{
	OneShotChannelImpl* channel = alloc::new(allocator, OneShotChannelImpl, { .allocator = allocator });
	defer catch alloc::free(allocator, channel);

	channel.allocator = allocator;
	channel.mu.init()!;
	defer catch channel.mu.destroy();
	channel.cond.init()!;

	return (OneShotChannel*)channel;
}

fn void OneShotChannel.destroy(&self)
{
	OneShotChannelImpl* channel = (OneShotChannelImpl*)self;
	channel.mu.destroy();
	channel.cond.destroy();
	alloc::free(channel.allocator, channel);
}

<*
 Send the one value for this channel.

 @return? thread::CHANNEL_CLOSED : "If the channel was closed or already has a value."
*>
fn void? OneShotChannel.push(&self, Type val)
{
	OneShotChannelImpl* channel = (OneShotChannelImpl*)self;

	channel.mu.lock();
	defer catch channel.mu.unlock();

	if (channel.closed || channel.sent) return thread::CHANNEL_CLOSED~;

	channel.value = val;
	channel.sent = true;
	channel.cond.broadcast();
	channel.mu.unlock();
}

<*
 Receive the value from the channel.

 Blocks until a value is sent or the channel is closed. If the value was sent
 before closing, it can still be received exactly once.

 @return? thread::CHANNEL_CLOSED : "If the channel was closed before sending, or the value was already consumed."
*>
fn Type? OneShotChannel.pop(&self)
{
	OneShotChannelImpl* channel = (OneShotChannelImpl*)self;

	channel.mu.lock();
	defer catch channel.mu.unlock();

	while (!channel.sent && !channel.closed)
	{
		channel.cond.wait(&channel.mu);
	}

	if (!channel.sent || channel.consumed)
	{
		return thread::CHANNEL_CLOSED~;
	}

	Type value = channel.value;
	channel.consumed = true;
	channel.mu.unlock();
	return value;
}

<*
 Close the channel and wake any blocked receivers.
*>
fn void OneShotChannel.close(&self)
{
	OneShotChannelImpl* channel = (OneShotChannelImpl*)self;

	channel.mu.lock();
	channel.closed = true;
	channel.cond.broadcast();
	channel.mu.unlock();
}
