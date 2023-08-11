module std::threads::os @if(env::POSIX);
import std::os::posix;
import libc;
def NativeMutex = Pthread_mutex_t;
def NativeConditionVariable = Pthread_cond_t;
def NativeThread = Pthread_t;
def NativeOnceFlag = Pthread_once_t;

fn void! NativeMutex.init(&mtx, MutexType type)
{
	Pthread_mutexattr_t attr;
	if (posix::pthread_mutexattr_init(&attr)) return ThreadFault.INIT_FAILED?;
	defer posix::pthread_mutexattr_destroy(&attr);
	if (type & thread::MUTEX_RECURSIVE)
	{
		if (posix::pthread_mutexattr_settype(&attr, posix::PTHREAD_MUTEX_RECURSIVE)) return ThreadFault.INIT_FAILED?;
	}
	if (posix::pthread_mutex_init(mtx, &attr)) return ThreadFault.INIT_FAILED?;
}

fn void! NativeMutex.destroy(&mtx)
{
	if (posix::pthread_mutex_destroy(mtx)) return ThreadFault.DESTROY_FAILED?;
}

fn void! NativeMutex.lock(&mtx)
{
	if (posix::pthread_mutex_lock(mtx)) return ThreadFault.LOCK_FAILED?;
}

fn void! NativeMutex.lock_timeoutout(&mtx, ulong ms)
{
	/* Try to acquire the lock and, if we fail, sleep for 5ms. */
	Errno result;
	while ((result = posix::pthread_mutex_trylock(mtx)) == errno::EBUSY)
	{
		if (!ms) break;
		ulong sleep = min(5, ms);
		if (!libc::nanosleep(&& TimeSpec { .s = 0, .ns = (CLong)sleep * 1000_000 }, null)) return ThreadFault.LOCK_FAILED?;
		ms -= sleep;
	}
	switch (result)
	{
		case errno::OK:
			return;
		case errno::EBUSY:
		case errno::ETIMEDOUT:
			return ThreadFault.LOCK_TIMEOUT?;
		default:
			return ThreadFault.LOCK_FAILED?;
	}
}

fn bool NativeMutex.try_lock(&mtx)
{
	return !posix::pthread_mutex_trylock(mtx);
}

fn void! NativeMutex.unlock(&mtx)
{
	if (posix::pthread_mutex_unlock(mtx)) return ThreadFault.UNLOCK_FAILED?;
}

fn void! NativeConditionVariable.init(&cond)
{
	if (posix::pthread_cond_init(cond, null)) return ThreadFault.INIT_FAILED?;
}

fn void! NativeConditionVariable.destroy(&cond)
{
	if (posix::pthread_cond_destroy(cond)) return ThreadFault.DESTROY_FAILED?;
}

fn void! NativeConditionVariable.signal(&cond)
{
	if (posix::pthread_cond_signal(cond)) return ThreadFault.SIGNAL_FAILED?;
}

fn void! NativeConditionVariable.broadcast(&cond)
{
	if (posix::pthread_cond_broadcast(cond)) return ThreadFault.SIGNAL_FAILED?;
}

fn void! NativeConditionVariable.wait(&cond, NativeMutex* mtx)
{
	if (posix::pthread_cond_wait(cond, mtx)) return ThreadFault.WAIT_FAILED?;
}

fn void! NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms)
{
	TimeSpec now;
	if (libc::timespec_get(&now, libc::TIME_UTC) != libc::TIME_UTC) return ThreadFault.WAIT_FAILED?;
	now.s += (Time_t)(ms / 1000);
	now.ns += (CLong)((ms % 1000) * 1000_000);
	switch (posix::pthread_cond_timedwait(cond, mtx, &now))
	{
		case errno::ETIMEDOUT:
			return ThreadFault.WAIT_TIMEOUT?;
		case errno::OK:
			return;
		default:
			return ThreadFault.WAIT_FAILED?;
	}
}


fn void* callback(void* arg) @private
{
	PosixThreadData *data = arg;
	return (void*)(iptr)data.thread_fn(data.arg);
}

fn void! NativeThread.create(&thread, ThreadFn thread_fn, void* arg)
{
	PosixThreadData *thread_data = malloc(PosixThreadData);
	*thread_data = { .thread_fn = thread_fn, .arg = arg };
	if (posix::pthread_create(thread, null, &callback, thread_data) != 0)
	{
		*thread = null;
		free(thread_data);
		return ThreadFault.INIT_FAILED?;
	}
}

fn void! NativeThread.detach(thread)
{
	if (!posix::pthread_detach(thread)) return ThreadFault.DETACH_FAILED?;
}

fn void native_thread_exit(int result)
{
	posix::pthread_exit((void*)(iptr)result);
}

fn NativeThread native_thread_current()
{
	return (NativeThread)posix::pthread_self();
}

fn bool NativeThread.equals(thread, NativeThread other)
{
	return (bool)posix::pthread_equal(thread, other);
}

fn int! NativeThread.join(thread)
{
	void *pres;
	if (posix::pthread_join(thread, &pres)) return ThreadFault.JOIN_FAILED?;
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

struct PosixThreadData @private
{
	ThreadFn thread_fn;
	void* arg;
}

fn void! native_sleep_nano(ulong nano)
{
	TimeSpec to = { .s = 0, .ns = (CLong)nano };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED?;
}

fn void! native_sleep_ms(ulong ms)
{
	TimeSpec to = { .s = (Time_t)(ms / 1000), .ns = (CLong)((ms % 1000) * 1000_000) };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED?;
}

fn void! native_sleep(double s)
{
	Time_t si = (Time_t)s;
	TimeSpec to = { .s = si, .ns = (CLong)((s - si) * 1000_000_000) };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED?;
}

