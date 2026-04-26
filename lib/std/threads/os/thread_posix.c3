module std::thread::os @if(env::POSIX);
import std::os::posix, std::time, libc;
import std::thread;

const ANDROID_LINUX = env::LINUX || env::ANDROID;

struct NativeMutex
{
	Pthread_mutex_t mutex;
	bool initialized;
}

alias NativeTimedMutex = NativeMutex;

alias NativeConditionVariable = Pthread_cond_t;

struct NativeThread
{
	Pthread_t pthread;
	ThreadFn thread_fn;
    void* arg;
    Sem_t sem @if (ANDROID_LINUX);
    Pid_t tid @if (ANDROID_LINUX);
}

alias NativeOnceFlag = Pthread_once_t;

<*
 @require !self.is_initialized() : "Mutex is already initialized"
 @ensure self.is_initialized()
*>
fn void? NativeMutex.init(&self, MutexType type)
{
	Pthread_mutexattr_t attr;
	if (posix::pthread_mutexattr_init(&attr)) return thread::INIT_FAILED~;
	defer posix::pthread_mutexattr_destroy(&attr);
	// TODO: make a fine grained error instead
	if (type.recursive)
	{
		if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_RECURSIVE)) return thread::INIT_FAILED~;
	}
	else
	{
		$if env::COMPILER_SAFE_MODE:
			if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_ERRORCHECK)) return thread::INIT_FAILED~;
		$endif
	}
	if (posix::pthread_mutex_init(&self.mutex, &attr)) return thread::INIT_FAILED~;
	self.initialized = true;
}

fn bool NativeMutex.is_initialized(&self)
{
	return self.initialized;
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
 @ensure !self.is_initialized()
*>
fn void NativeMutex.destroy(&self)
{
	if (Errno err = posix::pthread_mutex_destroy(&self.mutex)) abort("Error destroying mutex: %d", err);
	*self = {};
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn void NativeMutex.lock(&self)
{
	switch (posix::pthread_mutex_lock(&self.mutex))
	{
		case EINVAL: unreachable("Mutex invalid");
		case EDEADLK: abort("Mutex deadlock");
		case OK: return;
		default: unreachable("Unexpected error in lock");
	}
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn void? NativeMutex.lock_timeout(&self, long ms)
{
	/* Try to acquire the lock and, if we fail, sleep for 5ms. */
	Errno result;
	while ((result = posix::pthread_mutex_trylock(&self.mutex)) == EBUSY)
	{
		if (!ms) break;
		long sleep = min(5, ms);
		if (!libc::nanosleep(&&time::ms(ms).to_timespec(), null)) return thread::LOCK_TIMEOUT~;
		ms -= sleep;
	}
	switch (result)
	{
		case OK:
			return;
		case EBUSY:
		case ETIMEDOUT:
			return thread::LOCK_TIMEOUT~;
		default:
			return unreachable("Invalid lock %d", result);
	}
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn bool NativeMutex.try_lock(&self)
{
	return !posix::pthread_mutex_trylock(&self.mutex);
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn void NativeMutex.unlock(&self)
{
	if (Errno err = posix::pthread_mutex_unlock(&self.mutex)) abort("Failed to unlock mutex: %d", err);
}

fn void? NativeConditionVariable.init(&cond)
{
	if (posix::pthread_cond_init(cond, null)) return thread::INIT_FAILED~;
}

fn void NativeConditionVariable.destroy(&cond)
{
	if (Errno err = posix::pthread_cond_destroy(cond)) abort("Failed to destroy pthread_cond %d", err);
}

fn void NativeConditionVariable.signal(&cond)
{
	if (Errno err = posix::pthread_cond_signal(cond)) abort("Failed to signal %d", err);
}

fn void NativeConditionVariable.broadcast(&cond)
{
	if (Errno err = posix::pthread_cond_broadcast(cond)) abort("Failed to broadcast %d", err);
}

<*
 @require mtx.is_initialized()
*>
fn void NativeConditionVariable.wait(&cond, NativeMutex* mtx)
{
	if (Errno err = posix::pthread_cond_wait(cond, &mtx.mutex)) abort("Failed to wait %d", err);
}

<*
 @require mtx.is_initialized()
 @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, long ms)
{
	Time time = time::now() + time::ms(ms);
	return cond.wait_until(mtx, time) @inline;
}

<*
 @require mtx.is_initialized()
 @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_timeout_duration(&cond, NativeMutex* mtx, Duration duration)
{
  if (duration < time::DURATION_ZERO) return thread::WAIT_TIMEOUT~;
	Time time = time::now() + duration;
	return cond.wait_until(mtx, time) @inline;
}

<*
 @require mtx.is_initialized()
 @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_until(&cond, NativeMutex* mtx, Time time)
{
	switch (posix::pthread_cond_timedwait(cond, &mtx.mutex, &&time.to_timespec()))
	{
		case ETIMEDOUT:
			return thread::WAIT_TIMEOUT~;
		case OK:
			return;
		default:
			$if(env::OPENBSD):
				// TODO: Investigate why this doesn't work correctly on openbsd.
				return thread::WAIT_TIMEOUT~;
			$else
				abort("pthread_cond_timedwait failed, invalid value");
			$endif
	}
}

tlocal NativeThread current_thread @private;

fn void* callback(void* arg) @private
{
	NativeThread* thread = arg;
	current_thread = *thread;
	$if ANDROID_LINUX:
		thread.tid = (Pid_t)@syscall(libc::SYS_GETTID);
		os::sem_post(&thread.sem);
	$endif
	return (void*)(iptr)thread.thread_fn(thread.arg);
}

fn bool NativeThread.set_priority(&self, ThreadPriority priority)
{
	if (priority == DEFAULT) priority = NORMAL;
	$switch:
		$case env::EMSCRIPTEN:
			return true;
		$case ANDROID_LINUX:
			if (priority == REALTIME)
			{
				Pthread_sched_param param =  { .sched_priority = 1 };
				return !posix::pthread_setschedparam(self.pthread, libc::SCHED_FIFO, &param);
			}
			if (!self.tid) return false;
			const CInt[4] NICE_TABLE = { 19, 5, 0, -5 };
			return !libc::setpriority(libc::PRIO_PROCESS, (CInt)self.tid, NICE_TABLE[priority.ordinal - 1]);
		$case $defined(libc::SCHED_OTHER, libc::sched_get_priority_min):
			Pthread_sched_param param @noinit;
			if (priority == REALTIME)
			{
				$if env::DARWIN:
					param = { .sched_priority = libc::sched_get_priority_max(libc::SCHED_FIFO) / 2 };
				$else
					param = { .sched_priority = libc::sched_get_priority_max(libc::SCHED_FIFO) };
				$endif
				return !posix::pthread_setschedparam(self.pthread, libc::SCHED_FIFO, &param);
			}
			else
			{
				int low = libc::sched_get_priority_min(libc::SCHED_OTHER);
				int high = libc::sched_get_priority_max(libc::SCHED_OTHER);
				param = { .sched_priority = (low + ((high - low) * (priority.ordinal - 1)) / 4) };
			}
			return !posix::pthread_setschedparam(self.pthread, libc::SCHED_OTHER, &param);
		$default:
			return false;
	$endswitch
}

<*
 @require settings.stack_size >= 0
 @return? thread::INIT_FAILED, thread::INVALID_THREAD_SETTINGS
*>
fn void? NativeThread.create(&thread, ThreadFn thread_fn, void* arg, ThreadSettings settings)
{
	thread.thread_fn = thread_fn;
	thread.arg = arg;
	Pthread_attr_t attr;
	Pthread_attr_t* attr_ptr = null;
	defer if (attr_ptr) posix::pthread_attr_destroy(attr_ptr);
	if (settings.stack_size > 0)
	{
		if (posix::pthread_attr_init(&attr)) return thread::INIT_FAILED~;
		attr_ptr = &attr;
		if (settings.stack_size > 0)
		{
			if (posix::pthread_attr_setstacksize(attr_ptr, settings.stack_size)) return thread::INVALID_THREAD_SETTINGS~;
		}
	}
	$if ANDROID_LINUX:
		os::sem_init(&thread.sem, 0, 0);
		defer os::sem_destroy(&thread.sem);
	$endif
	if (posix::pthread_create(&thread.pthread, null, &callback, thread) != 0)
	{
		return thread::INIT_FAILED~;
	}
	$if ANDROID_LINUX:
		os::sem_wait(&thread.sem);
	$endif
	if (settings.priority != DEFAULT) thread.set_priority(settings.priority);
}

fn void NativeThread.detach(thread)
{
	if (Errno errno = posix::pthread_detach(thread.pthread)) abort("Thread detach failed: %d", errno);
}

fn void native_thread_exit(int result)
{
	posix::pthread_exit((void*)(iptr)result);
}


fn NativeThread native_thread_current()
{
	if (!current_thread.thread_fn)
	{
		current_thread = { .pthread = posix::pthread_self(), .thread_fn = fn(void*) => 0 };
	}
	return current_thread;
}

fn bool NativeThread.equals(thread, NativeThread other)
{
	return (bool)posix::pthread_equal(thread.pthread, other.pthread);
}

<*
 @return "the return value of the thread"
 @return? thread::THREAD_NOT_FOUND : "If the thread was not running"
*>
fn int? NativeThread.join(thread) @maydiscard
{
	void *pres;
	switch (posix::pthread_join(thread.pthread, &pres))
	{
		case OK: return (int)(iptr)pres;
		case EINVAL: unreachable("Thread is not joinable.");
		case EDEADLK: unreachable("Thread join from current thread.");
		case ESRCH: return thread::THREAD_NOT_FOUND~;
		default: unreachable("Thread join returned unexpected result");
	}
}

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	posix::pthread_once(flag, func);
}

fn void native_thread_yield()
{
	posix::sched_yield();
}

fn void? native_sleep_nano(NanoDuration nano)
{
	if (nano <= time::NANO_DURATION_ZERO) return;
	if (libc::nanosleep(&&nano.to_timespec(), null)) return thread::INTERRUPTED~;
}

