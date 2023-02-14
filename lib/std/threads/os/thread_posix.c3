module std::thread::os;
import libc;

$if (thread::THREAD_MODEL == ThreadModel.POSIX):

const PTHREAD_MUTEX_NORMAL = 0;
const PTHREAD_MUTEX_ERRORCHECK = 1;
const PTHREAD_MUTEX_RECURSIVE = 2;

typedef NativeMutex = PthreadMutex;
typedef NativeConditionVariable = PthreadCond;
typedef NativeThread = Pthread;
typedef NativeOnceFlag = PthreadOnce;

typedef Pthread = distinct void*;

$if (env::OS_TYPE == OsType.LINUX):
typedef PthreadMutex = distinct ulong[5];
typedef PthreadAttribute = distinct ulong[7];
typedef PthreadMutexAttribute = distinct uint;
typedef PthreadCondAttribute = distinct uint;
typedef PthreadCond = distinct ulong[6];
typedef PthreadOnce = distinct uint;
$else:
typedef PthreadMutex = distinct ulong[8];
typedef PthreadMutexAttribute = distinct ulong[2];
typedef PthreadAttribute = distinct ulong[8];
typedef PthreadCond = distinct ulong[6];
typedef PthreadCondAttribute = distinct ulong[8];
typedef PthreadOnce = distinct ulong[2];
$endif;

typedef PosixThreadFn = fn void*(void*);

extern fn int pthread_attr_destroy(PthreadAttribute*);
extern fn int pthread_attr_getdetachstate(PthreadAttribute*, int*);
extern fn int pthread_attr_init(PthreadAttribute*);
extern fn int pthread_mutex_init(PthreadMutex*, PthreadMutexAttribute*);
extern fn int pthread_mutex_destroy(PthreadMutex*);
extern fn int pthread_mutex_lock(PthreadMutex*);
extern fn int pthread_mutexattr_init(PthreadMutexAttribute*);
extern fn int pthread_mutexattr_destroy(PthreadMutexAttribute*);
extern fn int pthread_mutexattr_settype(PthreadMutexAttribute*, int);
extern fn int pthread_cond_init(PthreadCond*, PthreadCondAttribute*);
extern fn int pthread_cond_destroy(PthreadCond*);
extern fn int pthread_detach(Pthread);
extern fn int pthread_join(Pthread, void**);
extern fn int pthread_create(Pthread*, PthreadAttribute*, PosixThreadFn, void*);
extern fn Pthread pthread_self();
extern fn Errno pthread_mutex_trylock(PthreadMutex*);
extern fn Errno pthread_mutex_unlock(PthreadMutex*);
extern fn Errno pthread_cond_signal(PthreadCond*);
extern fn Errno pthread_cond_broadcast(PthreadCond*);
extern fn Errno pthread_cond_wait(PthreadCond*, PthreadMutex*);
extern fn Errno pthread_cond_timedwait(PthreadCond*, PthreadMutex*, TimeSpec*);
extern fn void pthread_exit(void*);
extern fn void pthread_once(PthreadOnce*, OnceFn);
extern fn int pthread_equal(Pthread this, Pthread other);
extern fn int sched_yield();

fn void! NativeMutex.init(NativeMutex* mutex, MutexType type)
{
	PthreadMutexAttribute attr;
	if (pthread_mutexattr_init(&attr)) return ThreadFault.INIT_FAILED!;
	defer pthread_mutexattr_destroy(&attr);
    if (type & thread::MUTEX_RECURSIVE)
    {
        if (pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE)) return ThreadFault.INIT_FAILED!;
    }
    if (pthread_mutex_init(mutex, &attr)) return ThreadFault.INIT_FAILED!;
}

fn void! NativeMutex.destroy(NativeMutex* mtx)
{
	if (pthread_mutex_destroy(mtx)) return ThreadFault.DESTROY_FAILED!;
}

fn void! NativeMutex.lock(NativeMutex* mtx)
{
	if (pthread_mutex_lock(mtx)) return ThreadFault.LOCK_FAILED!;
}

fn void! NativeMutex.lock_timeoutout(NativeMutex* mtx, ulong ms)
{
    /* Try to acquire the lock and, if we fail, sleep for 5ms. */
    Errno result;
    while ((result = pthread_mutex_trylock(mtx)) == errno::EBUSY)
    {
        if (!ms) break;
        ulong sleep = min(5, ms);
        if (!libc::nanosleep(&& TimeSpec { .s = 0, .ns = sleep * 1000_000 }, null)) return ThreadFault.LOCK_FAILED!;
        ms -= sleep;
    }
    switch (result)
    {
        case errno::OK:
            return;
        case errno::EBUSY:
        case errno::ETIMEDOUT:
            return ThreadFault.LOCK_TIMEOUT!;
        default:
            return ThreadFault.LOCK_FAILED!;
    }
}

fn bool NativeMutex.try_lock(NativeMutex* mtx)
{
	return !pthread_mutex_trylock(mtx);
}

fn void! NativeMutex.unlock(NativeMutex* mtx)
{
	if (pthread_mutex_unlock(mtx)) return ThreadFault.UNLOCK_FAILED!;
}

fn void! NativeConditionVariable.init(NativeConditionVariable* cond)
{
	if (pthread_cond_init(cond, null)) return ThreadFault.INIT_FAILED!;
}

fn void! NativeConditionVariable.destroy(NativeConditionVariable* cond)
{
	if (pthread_cond_destroy(cond)) return ThreadFault.DESTROY_FAILED!;
}

fn void! NativeConditionVariable.signal(NativeConditionVariable* cond)
{
	if (pthread_cond_signal(cond)) return ThreadFault.SIGNAL_FAILED!;
}

fn void! NativeConditionVariable.broadcast(NativeConditionVariable* cond)
{
	if (pthread_cond_broadcast(cond)) return ThreadFault.SIGNAL_FAILED!;
}

fn void! NativeConditionVariable.wait(NativeConditionVariable* cond, NativeMutex* mtx)
{
    if (pthread_cond_wait(cond, mtx)) return ThreadFault.WAIT_FAILED!;
}

fn void! NativeConditionVariable.wait_timeout(NativeConditionVariable* cond, NativeMutex* mtx, ulong ms)
{
	TimeSpec now;
	if (libc::timespec_get(&now, libc::TIME_UTC) != libc::TIME_UTC) return ThreadFault.WAIT_FAILED!;
	now.s += ms / 1000;
	now.ns += (ms % 1000) * 1000_000;
	switch (pthread_cond_timedwait(cond, mtx, &now))
	{
		case errno::ETIMEDOUT:
			return ThreadFault.WAIT_TIMEOUT!;
		case errno::OK:
			return;
		default:
			return ThreadFault.WAIT_FAILED!;
	}
}


fn void* callback(void* arg) @private
{
	PosixThreadData *data = arg;
	return (void*)(iptr)data.thread_fn(data.arg);
}

fn void! NativeThread.create(NativeThread* thread, ThreadFn thread_fn, void* arg)
{
	PosixThreadData *thread_data = mem::alloc(PosixThreadData);
	*thread_data = { .thread_fn = thread_fn, .arg = arg };
	if (pthread_create(thread, null, &callback, thread_data) != 0)
    {
        *thread = null;
        free(thread_data);
        return ThreadFault.INIT_FAILED!;
    }
}

fn void! NativeThread.detach(NativeThread thread)
{
	if (!pthread_detach(thread)) return ThreadFault.DETACH_FAILED!;
}

fn void native_thread_exit(int result)
{
	pthread_exit((void*)(iptr)result);
}

fn NativeThread native_thread_current()
{
	return pthread_self();
}

fn bool NativeThread.equals(NativeThread this, NativeThread other)
{
	return (bool)pthread_equal(this, other);
}

fn int! NativeThread.join(NativeThread thread)
{
	void *pres;
	if (pthread_join(thread, &pres)) return ThreadFault.JOIN_FAILED!;
	return (int)(iptr)pres;
}

fn void NativeOnceFlag.call_once(NativeOnceFlag* flag, OnceFn func)
{
	pthread_once(flag, func);
}

fn void native_thread_yield()
{
	sched_yield();
}

struct PosixThreadData @private
{
	ThreadFn thread_fn;
	void* arg;
}

fn void! native_sleep_nano(ulong nano)
{
	TimeSpec to = { .s = 0, .ns = nano };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED!;
}

fn void! native_sleep_ms(ulong ms)
{
	TimeSpec to = { .s = ms / 1000, .ns = (ms % 1000) * 1000_000 };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED!;
}

fn void! native_sleep(double s)
{
	ulong si = (ulong)s;
	TimeSpec to = { .s = si, .ns = (ulong)((s - si) * 1000_000_000) };
	if (libc::nanosleep(&to, null)) return ThreadFault.INTERRUPTED!;
}

$endif;