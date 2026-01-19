module std::thread::pool <SIZE>;
import std::thread;

struct ThreadPool
{
	Mutex mu;
	QueueItem[SIZE] queue;
	usz qindex;
	usz qworking;
	usz num_threads;
	bitstruct : char
	{
		bool initialized;
		bool stop;
		bool stop_now;
		bool joining;
	}

	Thread[SIZE] pool;
	ConditionVariable notify;
	ConditionVariable collect;
}

struct QueueItem @private
{
	ThreadFn func;
	void* arg;
}

<*
 @require !self.initialized : "ThreadPool must not be already initialized"
*>
fn void? ThreadPool.init(&self)
{
	defer catch self.destroy();
	*self = { .num_threads = SIZE, .initialized = true };
	self.mu.init()!;
	self.notify.init()!;
	self.collect.init()!;
	foreach (&thread : self.pool)
	{
		thread.create(&process_work, self)!;
		// The thread resources will be cleaned up when the thread exits.
		thread.detach()!;
	}
}

<*
 Join all threads in the pool.
*>
fn void? ThreadPool.join(&self) @maydiscard // Remove optional in 0.8.0
{
	if (self.initialized)
	{
		self.mu.lock();
		self.joining = true;
		self.notify.broadcast();
		self.collect.wait(&self.mu);
		self.joining = false;
		self.mu.unlock();
	}
}

<*
 Stop all the threads and cleanup the pool.
 Any pending work will be dropped.
*>
fn void? ThreadPool.destroy(&self) @maydiscard // Remove optional in 0.8.0
{
	self.@shutdown(self.stop_now);
}

<*
 Stop all the threads and cleanup the pool.
 Any pending work will be processed.
*>
fn void? ThreadPool.stop_and_destroy(&self) @maydiscard // Remove optional in 0.8.0
{
	self.@shutdown(self.stop);
}

macro void ThreadPool.@shutdown(&self, #stop) @private
{
	if (self.initialized)
	{
		self.mu.lock();
		#stop = true;
		self.notify.broadcast();
		self.mu.unlock();
		// Wait for all threads to shutdown.
		while (true)
		{
			self.mu.lock();
			defer self.mu.unlock();
			if (self.num_threads == 0)
			{
				break;
			}
			self.notify.signal();
		}
		self.mu.destroy();
		self.notify.destroy();
		self.collect.destroy();
		self.initialized = false;
	}
}

<*
 Push a new job to the pool.
 Returns whether the queue is full, in which case the job is ignored.
*>
fn void? ThreadPool.push(&self, ThreadFn func, void* arg) @maydiscard // Remove optional in 0.8.0
{
	while (true)
	{
		self.mu.lock();
		defer self.mu.unlock();
		if (self.qindex < SIZE)
		{
			self.queue[self.qindex] = { .func = func, .arg = arg };
			self.qindex++;
			self.qworking++;
			// Notify the threads that work is available.
			self.notify.broadcast();
			return;
		}
	}
}

fn int process_work(void* arg) @private
{
	ThreadPool* self = arg;
	while (true)
	{
		self.mu.lock();
		// Wait for work.
		while (self.qindex == 0)
		{
			if (self.joining && self.qworking == 0) self.collect.broadcast();
			if (self.stop)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock();
				return 0;
			}
			self.notify.wait(&self.mu);
			if (self.stop_now)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock();
				return 0;
			}
		}
		// Process the job.
		self.qindex--;
		QueueItem item = self.queue[self.qindex];
		self.mu.unlock();
		item.func(item.arg);
		self.mu.lock();
		self.qworking--;
		self.mu.unlock();
	}
}