module std::thread::os @if(env::POSIX);
import std::os::posix, std::time, libc;

struct NativeMutex
{
	Pthread_mutex_t mutex;
	bool initialized;
}

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
	if (posix::pthread_mutexattr_init(&attr)) return thread::INIT_FAILED?;
	defer posix::pthread_mutexattr_destroy(&attr);
	// TODO: make a fine grained error instead
	if (type & thread::MUTEX_RECURSIVE)
	{
		if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_RECURSIVE)) return thread::INIT_FAILED?;
	}
	else
	{
		$if env::COMPILER_SAFE_MODE:
			if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_ERRORCHECK)) return thread::INIT_FAILED?;
		$endif
	}
	if (posix::pthread_mutex_init(&self.mutex, &attr)) return thread::INIT_FAILED?;
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
fn void? NativeMutex.destroy(&self)
{
	if (posix::pthread_mutex_destroy(&self.mutex)) return thread::DESTROY_FAILED?;
	*self = {};
}

<*
 @require self.is_initialized() : "Mutex was not initialized"
*>
fn void? NativeMutex.lock(&self)
{
	if (posix::pthread_mutex_lock(&self.mutex)) return thread::LOCK_FAILED?;
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
		if (!libc::nanosleep(&&time::ms(ms).to_timespec(), null)) return thread::LOCK_FAILED?;
		ms -= sleep;
	}
	switch (result)
	{
		case errno::OK:
			return;
		case errno::EBUSY:
		case errno::ETIMEDOUT:
			return thread::LOCK_TIMEOUT?;
		default:
			return thread::LOCK_FAILED?;
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
fn void? NativeMutex.unlock(&self)
{
	if (posix::pthread_mutex_unlock(&self.mutex)) return thread::UNLOCK_FAILED?;
}

fn void? NativeConditionVariable.init(&cond)
{
	if (posix::pthread_cond_init(cond, null)) return thread::INIT_FAILED?;
}

fn void? NativeConditionVariable.destroy(&cond)
{
	if (posix::pthread_cond_destroy(cond)) return thread::DESTROY_FAILED?;
}

fn void? NativeConditionVariable.signal(&cond)
{
	if (posix::pthread_cond_signal(cond)) return thread::SIGNAL_FAILED?;
}

fn void? NativeConditionVariable.broadcast(&cond)
{
	if (posix::pthread_cond_broadcast(cond)) return thread::SIGNAL_FAILED?;
}

<*
 @require mtx.is_initialized()
*>
fn void? NativeConditionVariable.wait(&cond, NativeMutex* mtx)
{
	if (posix::pthread_cond_wait(cond, &mtx.mutex)) return thread::WAIT_FAILED?;
}

<*
 @require mtx.is_initialized()
*>
fn void? NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms)
{
	TimeSpec now;
	if (libc::timespec_get(&now, libc::TIME_UTC) != libc::TIME_UTC) return thread::WAIT_FAILED?;
	now.ns += (CLong)((ms % 1000) * 1000_000);
	now.s += (Time_t)(ms / 1000 + now.ns / 1000_000_000);
	now.ns = now.ns % 1000_000_000;
	switch (posix::pthread_cond_timedwait(cond, &mtx.mutex, &now))
	{
		case errno::ETIMEDOUT:
			return thread::WAIT_TIMEOUT?;
		case errno::OK:
			return;
		default:
			return thread::WAIT_FAILED?;
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
		return thread::INIT_FAILED?;
	}
}

fn void? NativeThread.detach(thread)
{
	if (posix::pthread_detach(thread.pthread)) return thread::DETACH_FAILED?;
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

fn int? NativeThread.join(thread)
{
	void *pres;
	if (posix::pthread_join(thread.pthread, &pres)) return thread::JOIN_FAILED?;
	return (int)(iptr)pres;
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
	if (nano <= 0) return;
	if (libc::nanosleep(&&nano.to_timespec(), null)) return thread::INTERRUPTED?;
}

