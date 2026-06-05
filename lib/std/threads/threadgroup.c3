module std::thread::experimental::threadgroup <Work, Result>;
import std::thread, std::os;
// WARNING! This is experimental and does not have proper stability guarantees,
// as such, it will change in upcoming releases. This is just a preview.
// Do not use in production!

alias ThreadWorkFn = fn Result?(Work work, void* context);
alias ThreadInitFn = fn void(void* init_context);
alias ThreadFinalizerFn = fn void(void* finalizer_context, void* init_context);
alias OutChannel = UnboundedChannel{ThreadGroupResult};

const bool NO_RESULT = Result == void;

enum ThreadGroupState : char
{
	NO_INIT,
	INIT,
	RUNNING,
	STOPPING
}

struct ThreadGroupResult
{
	fault excuse;
	Work work;
	Result result @if(!NO_RESULT);
}

fn Result? ThreadGroupResult.get(&self)
{
	if (self.excuse) return self.excuse~;
	$if !NO_RESULT:
		return self.result;
	$endif
}

struct ThreadGroup
{
	ThreadWorkFn work_fn;
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
		UnbufferedChannel{Work}* unbuffered;
		BufferedChannel{Work}* buffered;
	}
	OutChannel* outbound;
    Thread[] threads;
	sz pending;
	ThreadSettings thread_settings;
	ThreadGroupState state;
	bool is_buffered;
}

<*
 @param [&inout] allocator
 @param thread_count : "Number of threads for the work group, 0 for max(cpus - 1, 2)"
 @param queue_capacity : "Queue size"
 @param work_fn : "Work function to run"
 @param context : "Any common context to use"
 @param thread_settings : "Thread settings to apply"

 @require thread_count < 65536 : "Too many threads"
 @require thread_count >= 0 : "Thread count cannot be negative"
 @require queue_capacity >= 0 : "Queue capacity must be 0 or higher"
 @require self.state == NO_INIT : "Cannot initialize it again"
*>
fn void? ThreadGroup.init(&self, Allocator allocator, ThreadWorkFn work_fn, sz queue_capacity = 64, int thread_count = 0, void* context = null, ThreadSettings thread_settings = {})
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
		.work_fn = work_fn,
		.context = context,
		.allocator = allocator,
	};
	self.mx.init()!;
	defer catch self.mx.destroy();
	self.done.init()!;
	defer catch self.done.destroy();

	if (queue_capacity > 0)
	{
		self.is_buffered = true;
		self.buffered = channel::create_buffered{Work}(allocator, queue_capacity)!;
	}
	else
	{
		self.unbuffered = channel::create_unbuffered{Work}(allocator)!;
	}

	// Rollback on error:
	defer catch
	{
		if (self.is_buffered)
		{
			self.buffered.destroy();
		}
		else
		{
			self.unbuffered.destroy();
		}
	}

	self.threads = alloc::new_array(allocator, Thread, thread_count);
	defer catch alloc::free(allocator, self.threads);

	self.outbound = channel::create_unbounded{ThreadGroupResult}(allocator)!;
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
*>
fn void? ThreadGroup.add_blocking(&self, Work work)
{
	defer catch _thread_work_group_work_done(self);
	return _thread_group_push(self, work, non_blocking: false);
}

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
*>
fn void? ThreadGroup.try_add(&self, Work work)
{
	if (catch err = _thread_group_push(self, work, non_blocking: true))
	{
		_thread_work_group_work_done(self);
		if (err == thread::CHANNEL_BLOCKED) return thread::THREAD_QUEUE_FULL~;
		return err~;
	}
}

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
*>
macro void? _thread_group_push(ThreadGroup* self, Work work, bool non_blocking) @private
{
	_thread_work_group_add_pending(self);
	if (self.state == INIT) non_blocking = true;
	return self.is_buffered ? self.buffered.push(work, non_blocking: non_blocking) : self.unbuffered.push(work, non_blocking: non_blocking);
}

<*
  @require self.state != NO_INIT : "Expected the thread group initialized"
*>
fn void? ThreadGroup.add_or_run(&self, Work work)
{
	if (catch err = _thread_group_push(self, work, non_blocking: true))
	{
		defer _thread_work_group_work_done(self);
		if (err == thread::CHANNEL_BLOCKED)
		{
			_execute_work(self.outbound, work, self.work_fn, self.context);
			return;
		}
		return err~;
	}
}

fn ThreadGroupResult? ThreadGroup.next_result(&self)
{
	return self.outbound.pop(non_blocking: true);
}

fn ThreadGroupResult? ThreadGroup.wait_until_result(&self)
{
	return self.outbound.pop(non_blocking: false);
}
<*
 @require self.state == RUNNING : "Expected the thread group to be running"
*>
fn void ThreadGroup.join(&self, bool run_on_caller = false)
{
	if (run_on_caller)
	{
		while (true)
		{
			if (!_thread_work_group_has_pending(self)) return;
			Work? work = _thread_work_group_try_pop(self);
			if (catch err = work)
			{
				if (err != thread::CHANNEL_EMPTY) break;
				thread::yield();
				continue;
			}
			_execute_work(self.outbound, work, self.work_fn, self.context);
			_thread_work_group_work_done(self);
		}
	}

	self.mx.@in_lock()
	{
		while (self.pending != 0) self.done.wait(&self.mx);
	};
}

fn void ThreadGroup.destroy(&self, bool caller_help_finish = false)
{
	if (self.state == NO_INIT || self.state == STOPPING) return;

	self.join(caller_help_finish);

	self.state = STOPPING;

	if (self.is_buffered)
	{
		self.buffered.close();
	}
	else
	{
		self.unbuffered.close();
	}

	self.outbound.close();

	foreach (thread : self.threads)
	{
		thread.join();
	}
	if (self.is_buffered)
	{
		self.buffered.destroy();
	}
	else
	{
		self.unbuffered.destroy();
	}
	self.outbound.destroy();
	alloc::free(self.allocator, self.threads);
	self.done.destroy();
	self.mx.destroy();
	self.state = NO_INIT;
}

fn void _thread_work_group_add_pending(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		self.pending++;
	};
}

fn void _thread_work_group_work_done(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		if (--self.pending == 0) self.done.broadcast();
	};
}

fn bool _thread_work_group_has_pending(ThreadGroup* self) @private
{
	self.mx.@in_lock()
	{
		return self.pending != 0;
	};
}

fn Work? _thread_work_group_try_pop(ThreadGroup* self) @private
{
	return self.is_buffered ? self.buffered.pop(non_blocking: true) : self.unbuffered.pop(non_blocking: true);
}

macro _execute_work(OutChannel* buffer, Work work, ThreadWorkFn work_fn, void* context) @private
{
	$if NO_RESULT:
		(void)buffer.push({ .excuse = @catch(work_fn(work, context)), .work = work });
	$else
		Result? res = work_fn(work, context);
		if (catch err = res)
		{
			(void)buffer.push({ .excuse = err, .work = work });
		}
		else
		{
			(void)buffer.push({ .result = res, .work = work });
		}
	$endif
}
fn int _thread_worker_main(void* arg) @private
{
	ThreadGroup* self = arg;
	ThreadInitFn init_fn = self.init_fn;
	ThreadWorkFn work_fn = self.work_fn;
	void *context = self.context;
	ThreadFinalizerFn finalizer_fn = self.finalizer_fn;
	void *init_context = self.init_context;
	void *finalizer_context = self.finalizer_context;
	OutChannel* out = self.outbound;
	allocators::auto_create_temp = true;
	if (init_fn) init_fn(init_context);
	while (true)
	{
		Work? work_item = self.is_buffered ? self.buffered.pop() : self.unbuffered.pop();
		if (catch work_item) break;
		_execute_work(out, work_item, work_fn, context);
		_thread_work_group_work_done(self);
	}
	if (finalizer_fn) finalizer_fn(finalizer_context, init_context);
	allocators::destroy_temp_allocators();
	return 0;
}