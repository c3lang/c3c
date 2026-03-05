module std::thread::os @if(env::POSIX);
import std::os::posix, std::time, libc;
import libc::errno;
import std::thread;



struct NativeMutex
{
	Pthread_mutex_t mutex;
	bool initialized;
}

alias NativeTimedMutex = NativeMutex;

alias NativeConditionVariable = Pthread_cond_t;

struct NativeThread
{
	inline Pthread_t pthread;
	ThreadFn thread_fn;
    void* arg;
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
		case errno::EINVAL: unreachable("Mutex invalid");
		case errno::EDEADLK: abort("Mutex deadlock");
		case errno::OK: return;
		default: unreachable("Unexpected error in lock");
	}
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn void? NativeMutex.lock_timeout(&self, ulong ms)
{
	/* Try to acquire the lock and, if we fail, sleep for 5ms. */
	Errno result;
	while ((result = posix::pthread_mutex_trylock(&self.mutex)) == errno::EBUSY)
	{
		if (!ms) break;
		ulong sleep = min(5, ms);
		if (!libc::nanosleep(&&time::ms(ms).to_timespec(), null)) return thread::LOCK_TIMEOUT~;
		ms -= sleep;
	}
	switch (result)
	{
		case errno::OK:
			return;
		case errno::EBUSY:
		case errno::ETIMEDOUT:
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
fn void? NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms)
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
		case errno::ETIMEDOUT:
			return thread::WAIT_TIMEOUT~;
		case errno::OK:
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
	return (void*)(iptr)thread.thread_fn(thread.arg);
}

fn void? NativeThread.create(&thread, ThreadFn thread_fn, void* arg)
{
	thread.thread_fn = thread_fn;
	thread.arg = arg;
	if (posix::pthread_create(&thread.pthread, null, &callback, thread) != 0)
	{
		return thread::INIT_FAILED~;
	}
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
		case errno::OK: return (int)(iptr)pres;
		case errno::EINVAL: unreachable("Thread is not joinable.");
		case errno::EDEADLK: unreachable("Thread join from current thread.");
		case errno::ESRCH: return thread::THREAD_NOT_FOUND~;
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

