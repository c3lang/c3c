module std::thread::experimental::threadgroup <Task, Result>;
import std::thread, std::os, std::collections::list;
// WARNING! This is experimental and does not have proper stability guarantees,
// as such, it will change in upcoming releases. This is just a preview.
// Do not use in production!

alias ThreadTaskFn = fn Result(Task task, void* context);
alias ThreadInitFn = fn void(void* init_context);
alias ThreadFinalizerFn = fn void(void* finalizer_context, void* init_context);
alias OutChannel = UnboundedChannel{ThreadTaskResult};

enum ThreadGroupState : char
{
	NO_INIT,
	INIT,
	RUNNING,
	STOPPING
}


struct ThreadTaskResult
{
	Task task;
	Result result @if(!NO_RESULT);
}

struct ThreadGroup
{
	ThreadTaskFn task_fn;
	void* context;
	ThreadInitFn init_fn;
	void* init_context;
	ThreadFinalizerFn finalizer_fn;
	void* finalizer_context;
	Allocator allocator;
	Mutex mx;
    ConditionVariable done;
	union
	{
		UnbufferedChannel{Task}* unbuffered;
		BufferedChannel{Task}* buffered;
		UnboundedChannel{Task}* unbounded;
	}
	OutChannel* outbound;
    Thread[] threads;
	sz pending;
	ThreadSettings thread_settings;
    ThreadGroupQueueType queue_type;
	ThreadGroupState state;
}

<*
 @param [&inout] allocator
 @param thread_count : "Number of threads for the task group, 0 for max(cpus - 1, 2)"
 @param queue_capacity : "Queue size, if 0 - uses an unbounded queue"
 @param task_fn : "Task function to run"
 @param context : "Any common context to use"
 @param thread_settings : "Thread settings to apply"

 @require thread_count < 65536 : "Too many threads"
 @require thread_count >= 0 : "Thread count cannot be negative"
 @require queue_capacity >= 0 : "Queue capacity must be 0 or higher"
 @require self.state == NO_INIT : "Cannot initialize it again"
*>
fn void? ThreadGroup.init(&self, Allocator allocator, ThreadTaskFn task_fn, int thread_count = 0, sz queue_capacity = 0, void* context = null, ThreadSettings thread_settings = {})
{
	if (thread_count < 1)
	{
		$if $defined(os::num_cpu):
			thread_count = max(os::num_cpu() - 1, 2);
		$else
			thread_count = 2;
		$endif
	}
	self.allocator = allocator;
	defer try self.state = INIT;
	defer catch self.destroy();

	*self = {
		.task_fn = task_fn,
		.context = context,
		.allocator = allocator,
	};
	self.mx.init()!;
	defer catch self.mx.destroy();
	self.done.init()!;
	defer catch self.done.destroy();

	switch (queue_capacity)
	{
		case 0:
			self.queue_type = UNBOUNDED;
			self.unbounded = channel::create_unbounded{Task}(allocator)!;
		case 1:
			self.queue_type = SINGLE;
			self.unbuffered = channel::create_unbuffered{Task}(allocator)!;
		default:
			self.queue_type = BOUNDED;
			self.buffered = channel::create_buffered{Task}(allocator, queue_capacity)!;
	}

	// Rollback on error:
	defer catch _thread_group_destroy_input_channel(self);

	self.threads = alloc::new_array(allocator, Thread, thread_count);
	defer catch alloc::free(allocator, self.threads);

	self.outbound = channel::create_unbounded{ThreadTaskResult}(allocator)!;
}

<*
 @require self.state == INIT : "Expected the thread group initialized but not running"
*>
fn void ThreadGroup.set_init_fn(&self, ThreadInitFn thread_init_fn, void* init_context)
{
	self.init_fn = thread_init_fn;
	self.init_context = init_context;
}

<*
 @require self.state == INIT : "Expected the thread group initialized but not running"
*>
fn void ThreadGroup.set_thread_settings(&self, ThreadSettings settings)
{
	self.thread_settings = settings;
}

<*
 @require self.state == INIT : "Expected the thread group initialized but not running"
*>
fn void ThreadGroup.set_finalizer_fn(&self, ThreadFinalizerFn thread_finalizer_fn, void* finalizer_context)
{
	self.finalizer_fn = thread_finalizer_fn;
	self.finalizer_context = finalizer_context;
}

<*
 @require self.state == INIT : "Expected the thread group initialized but not running"
*>
fn void? ThreadGroup.start(&self)
{
	defer catch self.destroy();
	self.state = RUNNING;
	foreach (&thread : self.threads)
	{
		thread.create(&_thread_worker_main, self, self.thread_settings)!;
	}
}

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
  @require self.state != STOPPING : "The thread group is stopping"
  @require self.state == RUNNING || self.queue_type == UNBOUNDED : "Adding a task bounded/single queue requires start() first"
*>
fn void ThreadGroup.add_task(&self, Task task)
{
	defer catch _thread_group_task_done(self);
	_thread_group_push(self, task, non_blocking: false)!!;
}

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
  @require self.state != STOPPING : "The thread group is stopping"
*>
fn void? ThreadGroup.try_add_task(&self, Task task)
{
	if (catch err = _thread_group_push(self, task, non_blocking: true))
	{
		_thread_group_task_done(self);
		if (err == thread::CHANNEL_BLOCKED) return thread::THREAD_QUEUE_FULL~;
		return err~;
	}
}


<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
  @require self.state != STOPPING : "The thread group is stopping"
*>
fn void ThreadGroup.add_task_or_run(&self, Task task)
{
	if (catch err = _thread_group_push(self, task, non_blocking: true))
	{
		defer _thread_group_task_done(self);
		if (err == thread::CHANNEL_BLOCKED)
		{
			_thread_group_execute_work(self.outbound, task, self.task_fn, self.context);
			return;
		}
		err~!!;
	}
}

<*
 @require self.state != NO_INIT : "Expected the thread group initialized"
 @return "Get the next result if one exists"
*>
fn ThreadTaskResult? ThreadGroup.try_pop_result(&self)
{
	return self.outbound.try_pop();
}

<*
 @require self.state != NO_INIT : "Expected the thread group initialized"
 @return "Get the next result if one exists, blocking until it arrives"
*>
fn ThreadTaskResult? ThreadGroup.pop_result(&self)
{
	return self.outbound.pop();
}

<*
 @require self.state != NO_INIT : "The thread group is not initialized"
*>
fn List{ThreadTaskResult} ThreadGroup.drain(&self, Allocator allocator)
{
	List{ThreadTaskResult} list;
	list.init(allocator);
	while (try res = self.try_pop_result())
	{
		list.push(res);
	}
	return list;
}

<*
 @require self.state != NO_INIT : "The thread group is not initialized"
*>
fn List{Result} ThreadGroup.drain_values(&self, Allocator allocator) @if(!NO_RESULT)
{
	List{Result} list;
	list.init(allocator);
	while (try res = self.try_pop_result().result)
	{
		list.push(res);
	}
	return list;
}


<*
 @require self.state == RUNNING : "Expected the thread group to be running"
*>
fn List{ThreadTaskResult} ThreadGroup.collect(&self, Allocator allocator, bool caller_help_finish = false)
{
	self.join(caller_help_finish);
	return self.drain(allocator);
}

<*
 @require self.state == RUNNING : "Expected the thread group to be running"
*>
fn List{Result} ThreadGroup.collect_values(&self, Allocator allocator, bool caller_help_finish = false) @if(!NO_RESULT)
{
	self.join(caller_help_finish);
	return self.drain_values(allocator);
}

<*
 @require self.state == RUNNING : "Expected the thread group to be running"
*>
fn void ThreadGroup.join(&self, bool caller_help_finish = false)
{
	if (caller_help_finish)
	{
		while (true)
		{
			if (!_thread_group_has_pending(self)) return;
			Task? task = _thread_group_try_pop(self);
			if (catch err = task)
			{
				if (err != thread::CHANNEL_EMPTY) break;
				thread::yield();
				continue;
			}
			_thread_group_execute_work(self.outbound, task, self.task_fn, self.context);
			_thread_group_task_done(self);
		}
	}

	self.mx.@in_lock()
	{
		while (self.pending != 0) self.done.wait(&self.mx);
	};
}

<*
 @require self.state == RUNNING : "Expected the thread group to be running"
*>
fn void ThreadGroup.drop_results(&self)
{
	self.outbound.clear();
}

fn void ThreadGroup.destroy(&self, bool caller_help_finish = false)
{
	if (self.state == NO_INIT || self.state == STOPPING) return;

	self.join(caller_help_finish);

	self.state = STOPPING;

	switch (self.queue_type)
	{
		case BOUNDED: self.buffered.close();
		case SINGLE: self.unbuffered.close();
		case UNBOUNDED: self.unbounded.close();
	}

	self.outbound.close();

	foreach (thread : self.threads)
	{
		thread.join();
	}
	_thread_group_destroy_input_channel(self);
	self.outbound.destroy();
	alloc::free(self.allocator, self.threads);
	self.done.destroy();
	self.mx.destroy();
	self.state = NO_INIT;
}

fn void _thread_group_add_pending(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		self.pending++;
	};
}

fn void _thread_group_task_done(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		if (--self.pending == 0) self.done.broadcast();
	};
}

fn bool _thread_group_has_pending(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		return self.pending != 0;
	};
}

fn Task? _thread_group_try_pop(ThreadGroup* self) @private
{
	switch (self.queue_type)
	{
		case BOUNDED: return self.buffered.try_pop();
		case SINGLE: return self.unbuffered.try_pop();
		case UNBOUNDED: return self.unbounded.try_pop();
	}
}

fn void _thread_group_destroy_input_channel(ThreadGroup* self) @inline
{
	switch (self.queue_type)
	{
		case BOUNDED: self.buffered.destroy();
		case SINGLE: self.unbuffered.destroy();
		case UNBOUNDED: self.unbounded.destroy();
	}
}

macro _thread_group_execute_work(OutChannel* buffer, Task task, ThreadTaskFn task_fn, void* context) @private
{
	$if NO_RESULT:
		(void)buffer.push({ .task = task });
	$else
		(void)buffer.push({ .result = task_fn(task, context), .task = task});
	$endif
}

fn int _thread_worker_main(void* arg) @private
{
	ThreadGroup* self = arg;
	ThreadInitFn init_fn = self.init_fn;
	ThreadTaskFn task_fn = self.task_fn;
	void *context = self.context;
	ThreadFinalizerFn finalizer_fn = self.finalizer_fn;
	void *init_context = self.init_context;
	void *finalizer_context = self.finalizer_context;
	OutChannel* out = self.outbound;
	allocators::auto_create_temp = true;

	if (init_fn) init_fn(init_context);
	while (true)
	{
		Task? task @noinit;
		switch (self.queue_type)
		{
			case SINGLE: task = self.unbuffered.pop();
			case BOUNDED: task = self.buffered.pop();
			case UNBOUNDED: task = self.unbounded.pop();
		}
		if (catch task) break;
		_thread_group_execute_work(out, task, task_fn, context);
		_thread_group_task_done(self);
	}
	if (finalizer_fn) finalizer_fn(finalizer_context, init_context);
	allocators::destroy_temp_allocators();
	return 0;
}

enum ThreadGroupQueueType : char @private
{
	SINGLE,
	BOUNDED,
	UNBOUNDED
}

const bool NO_RESULT @private = Result == void;

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
*>
macro void? _thread_group_push(ThreadGroup* self, Task task, bool non_blocking) @private
{
	_thread_group_add_pending(self);
	switch (self.queue_type)
	{
		case BOUNDED: return non_blocking ? self.buffered.try_push(task) : self.buffered.push(task);
		case SINGLE: return non_blocking ? self.unbuffered.try_push(task) : self.unbuffered.push(task);
		case UNBOUNDED: return self.unbounded.push(task);
	}
}
