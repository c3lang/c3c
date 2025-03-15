module std::thread::event;
/*
import std::collections;

enum EventThreadState
{
	LEADER,
	WORKER,
	FOLLOWER,
	KILLED
}

struct Event
{
	int x;
	bool is_cancelled;
}
struct EventThreadTask
{
	EventHandler* target;
	Event*[] events;
}

typedef EventQueue = void;
typedef EventHandler = void;
typedef EventMultiQueue = void;

fn EventThreadTask EventQueue.wait_for_task(&self) => EventThreadTask {};

struct EventThread
{
	inline Thread thread;
	EventThreadState state;
	EventQueue* queue;
	EventThreadPool* pool;
	bool is_alive;
	String name;
}

// synchronized
fn void EventThread.assign_to_leader(&self)
{

	self.state = LEADER;
	//self.notify_all();
}

fn void EventThread.kill(&self)
{
	self.is_alive = false;
	/* interrupt */
}

fn void wait_for_leadership(EventThread* thread) @local
{
	// SYNCHRONICED
	while (thread.is_alive && thread.state != LEADER)
	{
		//thread.wait();
	}
}

// synchronized
fn int run_thread(void* arg) @local
{
	EventThread* thread = arg;
	EventThreadPool* pool = thread.pool;
	while (thread.state != KILLED)
	{
		wait_for_leadership(thread);
		if (!@volatile_load(thread.is_alive))
		{
			thread.state = KILLED;
			continue;
		}
		EventThreadTask? task = thread.queue.wait_for_task();
		if (catch task)
		{
			pool.notify_user_shutdown(thread);
			continue;
		}
		thread.state = WORKER;
		pool.notify_thread_start(thread);
		foreach (e : task.events)
		{
			run_event(task.target, e);
		}
		//task.target.finished_processing_event();
		thread.state = FOLLOWER;
		pool.notify_thread_end(thread);
	}
	return 0;
}

fn void run_event(EventHandler* handler, Event* event)
{
	//defer event.free();
	if (event.is_cancelled) return;
	// handler.process_event(event);
}

struct EventThreadPool
{
	Allocator allocator;
	String name;
	LinkedList{EventThread*} pool;
	EventThread* leader;
	bool is_alive;
	int busy_threads;
	void* monitor_context;
	List{EventThread*} threads;
	EventMultiQueue* queue;
}


// synchronized
fn void EventThreadPool.notify_thread_start(&self, EventThread* thread)
{
	self.busy_threads++;
	if (!self.is_alive) return;
	if (try follower = self.pool.pop())
	{
		follower.assign_to_leader();
		self.leader = follower;
		return;
	}
	self.leader = null;
}

// sync
fn void EventThreadPool.notify_thread_end(&self, EventThread* thread)
{
	self.busy_threads--;
	if (!self.is_alive)
	{
		thread.kill();
		self.leader = null;
		return;
	}
	if (self.leader)
	{
		self.pool.push(thread);
		return;
	}
	thread.assign_to_leader();
	self.leader = thread;
	return;
}

// sync
fn void EventThreadPool.notify_user_shutdown(&self, EventThread* thread)
{
	// monitor.notifyThreadPoolShutdown(this);
	remove_all_pool_threads(self);
	thread.kill();
	self.leader = null;
	self.is_alive = false;
}

fn void remove_all_pool_threads(EventThreadPool* pool) @local
{
	while (try thread = pool.pool.pop())
	{
		thread.kill();
	}
	if (pool.leader)
	{
		pool.leader.kill();
		pool.leader = null;
	}
}

<*
 @require size > 0 : "Must have at least one thread"
*>
fn EventThreadPool* EventThreadPool.init(&self, int size, String name, Allocator allocator, void* monitor_context)
{
	*self = { .is_alive = true, .name = name.copy(allocator), .monitor_context = monitor_context, .allocator = allocator };
	self.pool.init(allocator);
	self.threads.init(allocator: allocator);
	for (int i = 0; i < size; i++)
	{
		EventThread* thread = allocator::new(allocator, EventThread, { .is_alive = true, .state = FOLLOWER, .pool = null, .queue = null /* pool.getQueue() */, });
		thread.name = string::format("%s:%d", name, i, allocator: allocator);
		self.threads.push(thread);
		if (self.leader)
		{
			self.pool.push(thread);
		}
		else
		{
			thread.assign_to_leader();
			self.leader = thread;
		}
	}
	foreach (t : self.threads)
	{
		// t.start()
	}
	return self;
}

// sync
fn void EventThreadPool.destroy(&self)
{
	remove_all_pool_threads(self);
	self.is_alive = false;
}
