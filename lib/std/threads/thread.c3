module std::thread;
import std::thread::os;
import std::time;

typedef MutexType = int;

const MutexType MUTEX_PLAIN = 0;
const MutexType MUTEX_TIMED = 1;
const MutexType MUTEX_RECURSIVE = 2;

typedef Mutex = NativeMutex;
typedef TimedMutex = inline Mutex;
typedef RecursiveMutex = inline Mutex;
typedef TimedRecursiveMutex = inline Mutex;
typedef ConditionVariable = NativeConditionVariable;
typedef Thread = inline NativeThread;
typedef OnceFlag = NativeOnceFlag;
alias OnceFn = fn void();

alias ThreadFn = fn int(void* arg);

faultdef
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
	CHANNEL_CLOSED;

macro void? Mutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_PLAIN);
macro void? TimedMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_TIMED);
macro void? RecursiveMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_RECURSIVE);
macro void? TimedRecursiveMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, MUTEX_TIMED | MUTEX_RECURSIVE);
macro void? Mutex.destroy(&mutex) => NativeMutex.destroy((NativeMutex*)mutex);
macro void? Mutex.lock(&mutex) => NativeMutex.lock((NativeMutex*)mutex);
macro void? TimedMutex.lock_timeout(&mutex, ulong ms) => NativeMutex.lock_timeout((NativeMutex*)mutex, ms);
macro void? TimedRecursiveMutex.lock_timeout(&mutex, ulong ms) => NativeMutex.lock_timeout((NativeMutex*)mutex, ms);
macro bool Mutex.try_lock(&mutex) => NativeMutex.try_lock((NativeMutex*)mutex);
macro void? Mutex.unlock(&mutex) => NativeMutex.unlock((NativeMutex*)mutex);
macro void Mutex.@in_lock(&mutex; @body)
{
	(void)mutex.lock();
	defer (void)mutex.unlock();
	@body();
}

macro void? ConditionVariable.init(&cond) => NativeConditionVariable.init((NativeConditionVariable*)cond);
macro void? ConditionVariable.destroy(&cond) => NativeConditionVariable.destroy((NativeConditionVariable*)cond);
macro void? ConditionVariable.signal(&cond) => NativeConditionVariable.signal((NativeConditionVariable*)cond);
macro void? ConditionVariable.broadcast(&cond) => NativeConditionVariable.broadcast((NativeConditionVariable*)cond);
macro void? ConditionVariable.wait(&cond, Mutex* mutex)
{
	return NativeConditionVariable.wait((NativeConditionVariable*)cond, (NativeMutex*)mutex);
}
macro void? ConditionVariable.wait_timeout(&cond, Mutex* mutex, ulong ms)
{
	return NativeConditionVariable.wait_timeout((NativeConditionVariable*)cond, (NativeMutex*)mutex, ms);
}

macro void? Thread.create(&thread, ThreadFn thread_fn, void* arg) => NativeThread.create(thread, thread_fn, arg);
macro void? Thread.detach(thread) => NativeThread.detach(thread);
macro int? Thread.join(thread) => NativeThread.join(thread);
macro bool Thread.equals(thread, Thread other) => NativeThread.equals(thread, other);

macro void OnceFlag.call(&flag, OnceFn func) => NativeOnceFlag.call_once((NativeOnceFlag*)flag, func);

macro void yield() => os::native_thread_yield();
macro Thread current() => (Thread)os::native_thread_current();
macro void exit(int result)
{
	allocator::destroy_temp_allocators();
	os::native_thread_exit(result);
}
macro void? sleep(Duration d) @maydiscard => os::native_sleep_nano(d.to_nano());
macro void? sleep_ms(ulong ms) @maydiscard  => sleep(time::ms(ms));
macro void? sleep_ns(NanoDuration ns) @maydiscard  => os::native_sleep_nano(ns);
