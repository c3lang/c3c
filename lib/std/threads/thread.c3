module std::thread;

enum ThreadModel
{
	WIN32,
	POSIX
}
const ThreadModel THREAD_MODEL = env::OS_TYPE == OsType.WIN32 ? ThreadModel.WIN32 : ThreadModel.POSIX;

define MutexType = distinct int;

const MutexType MUTEX_PLAIN = 0;
const MutexType MUTEX_TIMED = 1;
const MutexType MUTEX_RECURSIVE = 2;

define Mutex = distinct NativeMutex;
define ConditionVariable = distinct NativeConditionVariable;
define Thread = distinct NativeThread;
define OnceFlag = distinct NativeOnceFlag;
define OnceFn = fn void();

define ThreadFn = fn int(void* arg);

fault ThreadFault
{
	INIT_FAILED,
	DESTROY_FAILED,
	LOCK_FAILED,
	LOCK_TIMEOUT,
	UNLOCK_FAILED,
	SIGNAL_FAILED,
	WAIT_FAILED,
	WAIT_TIMEOUT,
	DETACH_FAILED,
	JOIN_FAILED,
	INTERRUPTED,
}

macro void! Mutex.init(Mutex* mutex, MutexType type) = NativeMutex.init((NativeMutex*)mutex, type);
macro void! Mutex.destroy(Mutex* mutex) = NativeMutex.destroy((NativeMutex*)mutex);
macro void! Mutex.lock(Mutex* mutex) = NativeMutex.lock((NativeMutex*)mutex);
macro void! Mutex.lock_timeout(Mutex* mutex, ulong ms) = NativeMutex.lock((NativeMutex*)mutex, ms);
macro bool Mutex.try_lock(Mutex* mutex) = NativeMutex.try_lock((NativeMutex*)mutex);
macro bool Mutex.unlock(Mutex* mutex) = NativeMutex.unlock((NativeMutex*)mutex);

macro void! ConditionVariable.init(ConditionVariable* cond) = NativeConditionVariable.init((NativeConditionVariable*)cond);
macro void! ConditionVariable.destroy(ConditionVariable* cond) = NativeConditionVariable.destroy((NativeConditionVariable*)cond);
macro void! ConditionVariable.signal(ConditionVariable* cond) = NativeConditionVariable.signal((NativeConditionVariable*)cond);
macro void! ConditionVariable.broadcast(ConditionVariable* cond) = NativeConditionVariable.broadcast((NativeConditionVariable*)cond);
macro void! ConditionVariable.wait(ConditionVariable* cond, Mutex* mutex)
{
    return NativeConditionVariable.wait((NativeConditionVariable*)cond, (NativeMutex*)mutex);
}
macro void! ConditionVariable.wait_timeout(ConditionVariable* cond, Mutex* mutex, ulong timeout)
{
    return NativeConditionVariable.wait_timeout((NativeConditionVariable*)cond, (NativeMutex*)mutex, timeout);
}


macro void! Thread.create(Thread* thread, ThreadFn thread_fn, void* arg) = NativeThread.create((NativeThread*)thread, thread_fn, arg);
macro void! Thread.detach(Thread thread) = NativeThread.detach((NativeThread)thread);
macro int! Thread.join(Thread thread) = NativeThread.join((NativeThread)thread);
macro bool Thread.equals(Thread thread, Thread other) = NativeThread.equals((NativeThread)this, (NativeThread)other);

macro void OnceFlag.call_once(OnceFlag* flag, OnceFn func) = NativeOnceFlag.call_once((NativeOnceFlag*)flag, func);

macro void yield() = os::native_thread_yield();
macro Thread current() = os::native_thread_current();
macro void exit(int result) = os::native_thread_exit(result);
macro void! sleep(double s) @maydiscard = os::native_sleep(s);
macro void! sleep_ms(ulong ms) @maydiscard  = os::native_sleep_ms(ms);
macro void! sleep_ns(ulong ns) @maydiscard  = os::native_sleep_nano(ns);

