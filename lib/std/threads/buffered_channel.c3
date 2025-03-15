module std::thread::channel{Type};

typedef BufferedChannel = void*;

struct BufferedChannelImpl @private
{
	Allocator allocator;
	Mutex mu;
	bool closed;
	usz size;
	usz elems;

	usz sendx;
	usz send_waiting;
	ConditionVariable send_cond;

	usz readx;
	usz read_waiting;
	ConditionVariable read_cond;

	Type[*] buf;
}

fn void? BufferedChannel.init(&self, Allocator allocator, usz size = 1)
{
	BufferedChannelImpl* channel = allocator::new_with_padding(allocator, BufferedChannelImpl, Type.sizeof * size)!;
	defer catch allocator::free(allocator, channel);

	channel.allocator = allocator;
	channel.size = size;
	channel.elems = 0;
	channel.sendx = 0;
	channel.send_waiting = 0;
	channel.readx = 0;
	channel.read_waiting = 0;

	channel.mu.init()!;
	defer catch (void)channel.mu.destroy();
	channel.send_cond.init()!;
	defer catch (void)channel.send_cond.destroy();
	channel.read_cond.init()!;
	defer catch (void)channel.read_cond.destroy();

	*self = (BufferedChannel)channel;
}

fn void? BufferedChannel.destroy(&self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)(*self);

	fault err = @catch(channel.mu.destroy());
	err = @catch(channel.send_cond.destroy()) ?: err;
	err = @catch(channel.read_cond.destroy()) ?: err;
	allocator::free(channel.allocator, channel);

	*self = null;

	if (err) return err?;
}

fn void? BufferedChannel.push(self, Type val)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.lock()!;
	defer catch (void)channel.mu.unlock();

	// if channel is full -> wait
	while (channel.elems == channel.size && !channel.closed)
	{
		channel.send_waiting++;
		channel.send_cond.wait(&channel.mu)!;
		channel.send_waiting--;
	}

	// check if channel is closed
	if (channel.closed) return thread::CHANNEL_CLOSED?;

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
		channel.read_cond.signal()!;
	}

	channel.mu.unlock()!;
}

fn Type? BufferedChannel.pop(self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	channel.mu.lock()!;
	defer catch (void)channel.mu.unlock();

	// if chan is empty -> wait for sender
	while (channel.elems == 0 && !channel.closed)
	{
		channel.read_waiting++;
		channel.read_cond.wait(&channel.mu)!;
		channel.read_waiting--;
	}

	// check if chan is closed and empty
	if (channel.closed && channel.elems == 0)
	{
		return thread::CHANNEL_CLOSED?;
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
		channel.send_cond.signal()!;
	}

	channel.mu.unlock()!;

	return ret;
}

fn void? BufferedChannel.close(self)
{
	BufferedChannelImpl* channel = (BufferedChannelImpl*)self;

	fault err = @catch(channel.mu.lock());

	channel.closed = true;

	err = @catch(channel.read_cond.broadcast()) ?: err;
	err = @catch(channel.send_cond.broadcast()) ?: err;
	err = @catch(channel.mu.unlock()) ?: err;

	if (err) return err?;
}

