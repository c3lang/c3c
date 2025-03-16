module std::thread::channel {Type};

typedef UnbufferedChannel = void*;

struct UnbufferedChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	Type buf;
	bool closed;

	Mutex send_mu;
	usz send_waiting;
	ConditionVariable send_cond;

	Mutex read_mu;
	usz read_waiting;
	ConditionVariable read_cond;
}

fn void? UnbufferedChannel.init(&self, Allocator allocator)
{
	UnbufferedChannelImpl* channel = allocator::alloc(allocator, UnbufferedChannelImpl);
	defer catch (void)allocator::free(allocator, channel);

	channel.allocator = allocator;
	channel.send_waiting = 0;
	channel.read_waiting = 0;

	channel.mu.init()!;
	defer catch (void)channel.mu.destroy();
	channel.send_mu.init()!;
	defer catch (void)channel.send_mu.destroy();
	channel.send_cond.init()!;
	defer catch (void)channel.send_cond.destroy();
	channel.read_mu.init()!;
	defer catch (void)channel.read_mu.destroy();
	channel.read_cond.init()!;

	*self = (UnbufferedChannel)channel;
}

fn void? UnbufferedChannel.destroy(&self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)(*self);

	fault err = @catch(channel.mu.destroy());
	err = @catch(channel.send_mu.destroy()) ?: err;
	err = @catch(channel.send_cond.destroy()) ?: err;
	err = @catch(channel.read_mu.destroy()) ?: err;
	err = @catch(channel.read_cond.destroy()) ?: err;
	allocator::free(channel.allocator, channel);

	if (err) return err?;

	*self = null;
}

fn void? UnbufferedChannel.push(self, Type val)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	channel.mu.lock()!;
	defer catch (void)channel.mu.unlock();
	channel.send_mu.lock()!;
	defer catch (void)channel.send_mu.unlock();

	if (channel.closed)
	{
		return thread::CHANNEL_CLOSED?;
	}

	// store value in the buffer
	channel.buf = val;
	// show that we are waiting for reader
	channel.send_waiting++;

	// if reader is already waiting for us -> awake him
	if (channel.read_waiting > 0)
	{
		channel.read_cond.signal()!;
	}

	// wait until reader takes value from buffer
	channel.send_cond.wait(&channel.mu)!;

	if (channel.closed) return thread::CHANNEL_CLOSED?;

	channel.mu.unlock()!;
	channel.send_mu.unlock()!;
}

fn Type? UnbufferedChannel.pop(self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	channel.mu.lock()!;
	defer catch (void)channel.mu.unlock();
	channel.read_mu.lock()!;
	defer catch (void)channel.read_mu.unlock();

	// if no one is waiting, then there is nothing in the buffer
	while (!channel.closed && channel.send_waiting == 0)
	{
		channel.read_waiting++;
		channel.read_cond.wait(&channel.mu)!;
		channel.read_waiting--;
	}

	if (channel.closed) return thread::CHANNEL_CLOSED?;

	// take value from buffer
	Type ret = channel.buf;

	// awake sender
	channel.send_waiting--;
	channel.send_cond.signal()!;

	channel.mu.unlock()!;
	channel.read_mu.unlock()!;

	return ret;
}

fn void? UnbufferedChannel.close(self)
{
	UnbufferedChannelImpl* channel = (UnbufferedChannelImpl*)self;

	fault err = @catch(channel.mu.lock());

	channel.closed = true;

	err = @catch(channel.read_cond.broadcast()) ?: err;
	err = @catch(channel.send_cond.broadcast()) ?: err;
	err = @catch(channel.mu.unlock()) ?: err;

	if (err) return err?;
}