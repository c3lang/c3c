module std::thread @if($feature(NEW_THREADPOOL));
import std::collections::deque, std::time, std::atomic::types;

faultdef POOL_SHUTDOWN, POOL_SPAWN_FAILED;

alias ThreadPoolTaskFn = fn void(void* arg);

struct ThreadPool
{
	Allocator allocator;

	Mutex mx;
	ConditionVariable work_available;   // Workers wait here for work.
	ConditionVariable space_available;  // Submitters wait here when the queue is full.

	Deque{PoolTask} queue;
	sz queue_capacity;                  // 0 = unbounded.

	PoolSlot[] slots;
	int free_slot;
	int reuse_slot;
	int max_threads;
	int alive_count;
	int idle_count;

	Duration keep_alive;                // 0 = idle workers never exit.
	ThreadSettings thread_settings;

	ThreadPoolState state;
	bool drop_pending;
}

enum ThreadPoolState
{
	NO_INIT,
	RUNNING,
	STOPPING
}

<*
 Initialize a ThreadPool. Workers are not spawned until the first submission.

 @param [&inout] allocator : "Allocator for the slot table and the work queue"
 @param max_threads : "Maximum number of worker threads alive at any one time"
 @param queue_capacity : "Pending jobs allowed while all workers are busy. 0 = unbounded"
 @param keep_alive : "Idle workers exit after this duration. 0 = workers never exit on idle"
 @param thread_settings : "Settings applied to each spawned worker thread"

 @require self.state == NO_INIT : "ThreadPool was already initialized"
 @require max_threads > 0 : "max_threads must be at least 1"
 @require max_threads < 0x10000 : "max_threads must be less than 65536"
 @require queue_capacity >= 0 : "queue_capacity must be 0 or higher"
 @require keep_alive >= (Duration)0 : "keep_alive must be 0 or higher"
*>
fn void? ThreadPool.init(&self, Allocator allocator, int max_threads, sz queue_capacity = 0, Duration keep_alive = time::MIN, ThreadSettings thread_settings = {})
{
	*self = {
		.allocator = allocator,
		.max_threads = max_threads,
		.queue_capacity = queue_capacity,
		.keep_alive = keep_alive,
		.thread_settings = thread_settings,
		.free_slot = 0,
		.reuse_slot = -1,
		.state = RUNNING,
	};
	self.mx.init()!;
	defer catch self.mx.destroy();
	self.work_available.init()!;
	defer catch self.work_available.destroy();
	self.space_available.init()!;
	defer catch self.space_available.destroy();

	self.queue.init(allocator, 16);
	defer catch self.queue.free();

	self.slots = alloc::alloc_array(allocator, PoolSlot, max_threads);
	foreach (int index, &slot : self.slots)
	{
		*slot = { .pool = self, .index = index, .next_slot = index + 1};
	}
	self.slots[^1].next_slot = -1;
}

<*
 Eagerly spawn up to `count` workers (capped at max_threads) so they are ready
 before the first submission.

 @param count : "The number of workers to spin up"
 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn void? ThreadPool.prewarm(&self, int count)
{
	self.mx.@in_lock()
	{
		while (self.alive_count < count && self.alive_count < self.max_threads)
		{
			_thread_pool_spawn_worker_in_lock(self)!;
		}
	};
}

<*
 Submit work to the pool. Blocks if the queue is full (only possible with a bounded
 queue). The caller owns the lifetime of `arg`; it must remain valid until the work
 function has finished using it.

 @param task_fn : "Function to run on a worker thread"
 @param [inout] arg : "Pointer passed to func"

 @return? thread::POOL_SHUTDOWN : "If the pool is shutting down"
 @return? thread::POOL_SPAWN_FAILED : "If a new worker was needed and could not be created"

 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn void? ThreadPool.add_task(&self, ThreadPoolTaskFn task_fn, void* arg)
{
	self.mx.@in_lock()
	{
		if (self.state == STOPPING) return thread::POOL_SHUTDOWN~;
		while (self.queue_capacity > 0 && self.queue.len() >= self.queue_capacity)
		{
			self.space_available.wait(&self.mx);
			if (self.state == STOPPING) return thread::POOL_SHUTDOWN~;
		}
		self.queue.push({ .task_fn = task_fn, .arg = arg });
		defer catch (void)self.queue.pop();
		return _thread_pool_wake_or_spawn_in_lock(self);
	};
}

<*
 Submit work without blocking. Returns THREAD_QUEUE_FULL if the queue is at capacity.

 @param task_fn : "Function to run on a worker thread"
 @param [inout] arg : "Pointer passed to func"

 @return? thread::THREAD_QUEUE_FULL : "If the queue is at capacity"
 @return? thread::POOL_SHUTDOWN : "If the pool is shutting down"
 @return? thread::POOL_SPAWN_FAILED : "If a new worker was needed and could not be created"

 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn void? ThreadPool.try_add_task(&self, ThreadPoolTaskFn task_fn, void* arg)
{
	self.mx.@in_lock()
	{
		if (self.state == STOPPING) return thread::POOL_SHUTDOWN~;
		if (self.queue_capacity > 0 && self.queue.len() >= self.queue_capacity) return thread::THREAD_QUEUE_FULL~;
		self.queue.push({ .task_fn = task_fn, .arg = arg });
		defer catch (void)self.queue.pop();
		return _thread_pool_wake_or_spawn_in_lock(self);
	};
}

<*
 Stop accepting new work, let the queued jobs finish, then join all workers and
 release resources. Safe to call on an uninitialized or already shut-down pool.
*>
fn void ThreadPool.destroy(&self) => _thread_pool_shutdown(self, false);

<*
 Stop accepting new work, discard any queued jobs, then join all workers and release
 resources. Jobs already running are allowed to finish. Safe to call on an
 uninitialized or already shut-down pool.
*>
fn void ThreadPool.destroy_now(&self) => _thread_pool_shutdown(self, true);

<*
 The number of jobs currently waiting in the queue.

 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn sz ThreadPool.queue_count(&self)
{
	self.mx.@in_lock()
	{
		return self.queue.len();
	};
}

<*
 The number of workers currently executing a job (neither idle nor exited).

 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn int ThreadPool.busy_count(&self)
{
	self.mx.@in_lock()
	{
		return self.alive_count - self.idle_count;
	};
}

<*
 The number of worker threads currently alive (busy or idle).

 @require self.state != NO_INIT : "ThreadPool is not initialized"
*>
fn int ThreadPool.worker_count(&self)
{
	self.mx.@in_lock()
	{
		return self.alive_count;
	};
}

// --- private ---

fn void? _thread_pool_wake_or_spawn_in_lock(ThreadPool* self) @private
{
	if (self.idle_count > 0)
	{
		self.work_available.signal();
		return;
	}
	if (self.alive_count >= self.max_threads) return;
	return _thread_pool_spawn_worker_in_lock(self);
}

fn void? _thread_pool_spawn_worker_in_lock(ThreadPool* self) @private
{
	PoolSlot* slot @noinit;
	switch
	{
		case self.reuse_slot >= 0:
			// The worker has already returned and is not waiting on the lock, so joining
			// it here cannot deadlock.
			slot = &self.slots[self.reuse_slot];
			self.reuse_slot = slot.next_slot;
			slot.next_slot = -1;
			slot.thread.join();
		case self.free_slot >= 0:
			slot = &self.slots[self.free_slot];
			self.free_slot = slot.next_slot;
			slot.next_slot = -1;
		default:
			return thread::POOL_SPAWN_FAILED~;
	}
	if (catch slot.thread.create(&_thread_pool_worker_main, slot, self.thread_settings))
	{
		slot.next_slot = self.free_slot;
		self.free_slot = slot.index;
		return thread::POOL_SPAWN_FAILED~;
	}
	self.alive_count++;
}

fn void _thread_pool_shutdown(ThreadPool* self, bool drop) @private
{
	if (self.state == NO_INIT) return;

	self.mx.lock();
	if (self.state == STOPPING)
	{
		self.mx.unlock();
		return;
	}
	self.state = STOPPING;
	self.drop_pending = drop;
	if (drop) self.queue.clear();
	self.work_available.broadcast();
	self.space_available.broadcast();

	// Snapshot the slots that need joining.
	int free = self.free_slot;
	while (free > -1)
	{
		self.slots[free].no_join = true;
		free = self.slots[free].next_slot;
	}
	self.mx.unlock();

	foreach (&slot : self.slots)
	{
		if (!slot.no_join) slot.thread.join();
	}

	// Every worker has fully terminated; tear the pool down.
	self.queue.free();
	alloc::free(self.allocator, self.slots);
	self.slots = {};
	self.space_available.destroy();
	self.work_available.destroy();
	self.mx.destroy();
	self.state = NO_INIT;
}

fn int _thread_pool_worker_main(void* arg) @private
{
	PoolSlot* slot = arg;
	ThreadPool* pool = slot.pool;
	allocators::auto_create_temp = true;
	while (true)
	{
		pool.mx.lock();
		pool.idle_count++;

		bool timed_out = false;
		while (pool.queue.is_empty() && pool.state != STOPPING)
		{
			if (pool.keep_alive > (Duration)0)
			{
				Time deadline = time::now() + pool.keep_alive;
				if (catch pool.work_available.wait_until(&pool.mx, deadline))
				{
					timed_out = true;
					break;
				}
			}
			else
			{
				pool.work_available.wait(&pool.mx);
			}
		}

		pool.idle_count--;

		bool exit_for_shutdown = pool.state == STOPPING && (pool.drop_pending || pool.queue.is_empty());
		bool exit_for_idle = timed_out && pool.queue.is_empty() && pool.state != STOPPING;

		if (exit_for_shutdown || exit_for_idle)
		{
			slot.next_slot = pool.reuse_slot;
			pool.reuse_slot = slot.index;
			pool.alive_count--;
			pool.mx.unlock();
			break;
		}

		PoolTask work = pool.queue.pop_first()!!;
		bool bounded = pool.queue_capacity > 0;
		pool.mx.unlock();

		if (bounded) pool.space_available.signal();

		work.task_fn(work.arg);
	}

	allocators::destroy_temp_allocators();
	return 0;
}

struct PoolSlot @private
{
	Thread thread;
	ThreadPool *pool;
	int index;
	int next_slot;
	bool no_join;
}

struct PoolTask @private
{
	ThreadPoolTaskFn task_fn;
	void* arg;
}
