module std::thread;
import std::thread::os;
import std::time;

bitstruct MutexType : int
{
	bool timed;
	bool recursive;
}

typedef Mutex = NativeMutex;
typedef RecursiveMutex = inline Mutex;
typedef TimedMutex = NativeTimedMutex;
typedef TimedRecursiveMutex = inline TimedMutex;
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

macro void? Mutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, {});
macro bool Mutex.is_initialized(mutex) => ((NativeMutex*)&mutex).is_initialized();
macro void? RecursiveMutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, {.recursive});
macro void? Mutex.destroy(&mutex) => NativeMutex.destroy((NativeMutex*)mutex);
macro void? Mutex.lock(&mutex) => NativeMutex.lock((NativeMutex*)mutex);
macro bool  Mutex.try_lock(&mutex) => NativeMutex.try_lock((NativeMutex*)mutex);
macro void? Mutex.unlock(&mutex) => NativeMutex.unlock((NativeMutex*)mutex);

macro void? TimedMutex.init(&mutex) => NativeTimedMutex.init((NativeTimedMutex*)mutex, {.timed});
macro void? TimedRecursiveMutex.init(&mutex) => NativeTimedMutex.init((NativeTimedMutex*)mutex, {.timed, .recursive});
macro void? TimedMutex.destroy(&mutex) => NativeTimedMutex.destroy((NativeTimedMutex*)mutex);
macro void? TimedMutex.lock(&mutex) => NativeTimedMutex.lock((NativeTimedMutex*)mutex);
macro void? TimedMutex.lock_timeout(&mutex, ulong ms) => NativeTimedMutex.lock_timeout((NativeTimedMutex*)mutex, ms);
macro bool  TimedMutex.try_lock(&mutex) => NativeTimedMutex.try_lock((NativeTimedMutex*)mutex);
macro void? TimedMutex.unlock(&mutex) => NativeTimedMutex.unlock((NativeTimedMutex*)mutex);

macro void fence(AtomicOrdering $ordering) @safemacro
{
	$$fence($ordering.ordinal);
}

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
<*
 @require $defined(Duration d = #ms_or_duration) ||| $defined(ulong l = #ms_or_duration)
 @return? thread::WAIT_TIMEOUT, thread::WAIT_FAILED
*>
macro void? ConditionVariable.wait_timeout(&cond, Mutex* mutex, #ms_or_duration) @safemacro
{
	$if $defined(*&&(Duration){} = #ms_or_duration):
		return NativeConditionVariable.wait_timeout_duration((NativeConditionVariable*)cond, (NativeMutex*)mutex, #ms_or_duration);
	$else
		return NativeConditionVariable.wait_timeout((NativeConditionVariable*)cond, (NativeMutex*)mutex, #ms_or_duration);
	$endif
}

<*
 @return? thread::WAIT_TIMEOUT, thread::WAIT_FAILED
*>
macro void? ConditionVariable.wait_until(&cond, Mutex* mutex, Time time)
{
	return NativeConditionVariable.wait_until((NativeConditionVariable*)cond, (NativeMutex*)mutex, time);
}

<*
 Create and start a thread.

 @require thread_fn != null : "A non null thread function is required"
*>
macro void? Thread.create(&thread, ThreadFn thread_fn, void* arg)
{
	return NativeThread.create(thread, thread_fn, arg);
}

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
