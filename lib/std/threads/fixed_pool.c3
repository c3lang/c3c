module std::thread;
faultdef THREAD_QUEUE_FULL;

module std::thread::threadpool @if (env::POSIX || env::WIN32);
import std::thread;

// Please do not use this one in production.

alias ThreadPoolFn = fn void(any[] args);

struct FixedThreadPool
{
	Mutex mu;
	QueueItem[] queue;
	usz qindex;
	usz num_threads;
	bitstruct : char {
		bool initialized;
		bool stop;
		bool stop_now;
	}
	Thread[] pool;
	ConditionVariable notify;
}

struct QueueItem @private
{
	ThreadPoolFn func;
	any[] args;
}

<*
 @require !self.initialized : "ThreadPool must not be already initialized"
 @require threads > 0 && threads < 0x1000 : `Threads should be greater than 0 and less than 0x1000`
 @require queue_size < 0x10000 : `Queue size must be less than 65536`
*>
fn void? FixedThreadPool.init(&self, usz threads, usz queue_size = 0)
{
	if (queue_size == 0) queue_size = threads * 32;
	defer catch @ok(self.destroy());
	assert(queue_size > 0);
	*self = {
		.num_threads = threads,
		.initialized = true,
		.queue = mem::alloc_array(QueueItem, queue_size),
		.pool = mem::new_array(Thread, threads)
	};
	self.mu.init()!;
	self.notify.init()!;
	foreach (&thread : self.pool)
	{
		thread.create(&process_work, self)!;
		// The thread resources will be cleaned up when the thread exits.
		thread.detach()!;
	}
}

<*
 Stop all the threads and cleanup the pool.
 Any pending work will be dropped.
*>
fn void? FixedThreadPool.destroy(&self)
{
	return self.@shutdown(stop_now);
}

<*
 Stop all the threads and cleanup the pool.
 Any pending work will be processed.
*>
fn void? FixedThreadPool.stop_and_destroy(&self)
{
	return self.@shutdown(stop);
}

macro void? FixedThreadPool.@shutdown(&self, #stop) @private
{
	if (self.initialized)
	{
		self.mu.lock()!;
		self.#stop = true;
		self.notify.broadcast()!;
		self.mu.unlock()!;
		// Wait for all threads to shutdown.
		while (true)
		{
			self.mu.lock()!;
			defer self.mu.unlock()!!;
			if (self.num_threads == 0)
			{
				break;
			}
			self.notify.signal()!;
		}
		self.mu.destroy()!;
		self.initialized = false;
		while (self.qindex)
		{
			free_qitem(self.queue[--self.qindex]);
		}
		free(self.queue);
		self.queue = {};
	}
}

<*
 Push a new job to the pool.
 return Excuse if the queue is full, in which case the job is ignored.
*>
fn void? FixedThreadPool.push(&self, ThreadPoolFn func, args...)
{
	self.mu.lock()!;
	defer self.mu.unlock()!!;
	if (self.qindex == self.queue.len) return thread::THREAD_QUEUE_FULL?;
	any[] data;
	if (args.len)
	{
		data = mem::alloc_array(any, args.len);
		foreach (i, arg : args) data[i] = allocator::clone_any(mem, arg);
	}
	self.queue[self.qindex] = { .func = func, .args = data };
	self.qindex++;
	defer catch
	{
		free_qitem(self.queue[--self.qindex]);
	}
	// Notify the threads that work is available.
	self.notify.broadcast()!;
}

fn int process_work(void* self_arg) @private
{
	FixedThreadPool* self = self_arg;
	while (true)
	{
		self.mu.lock()!!;
		if (self.stop_now)
		{
			// Shutdown requested.
			self.num_threads--;
			self.mu.unlock()!!;
			return 0;
		}
		// Wait for work.
		while (self.qindex == 0)
		{
			if (self.stop)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock()!!;
				return 0;
			}
			self.notify.wait(&self.mu)!!;
			if (self.stop_now)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock()!!;
				return 0;
			}
		}
		// Process the job.
		self.qindex--;
		QueueItem item = self.queue[self.qindex];
		self.mu.unlock()!!;
		defer free_qitem(item);
		item.func(item.args);
	}
}

fn void free_qitem(QueueItem item) @private
{
	foreach (arg : item.args) free(arg.ptr);
	free(item.args);
}
