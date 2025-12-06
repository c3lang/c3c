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
	LOCK_TIMEOUT,
	WAIT_TIMEOUT,
	THREAD_NOT_FOUND,
	INTERRUPTED,
	CHANNEL_CLOSED;

faultdef
	DETACH_FAILED @deprecated,
	UNLOCK_FAILED @deprecated,
	DESTROY_FAILED @deprecated,
	SIGNAL_FAILED @deprecated,
	JOIN_FAILED @deprecated,
	LOCK_FAILED @deprecated,
	WAIT_FAILED @deprecated;

macro void? Mutex.init(&mutex) => NativeMutex.init((NativeMutex*)mutex, {});
macro bool Mutex.is_initialized(mutex) => ((NativeMutex*)&mutex).is_initialized();
macro void? RecursiveMutex.init(&mutex) @maydiscard => NativeMutex.init((NativeMutex*)mutex, {.recursive});
macro void? Mutex.destroy(&mutex) @maydiscard => NativeMutex.destroy((NativeMutex*)mutex); // Remove optional in 0.8.0
macro void? Mutex.lock(&mutex) @maydiscard => NativeMutex.lock((NativeMutex*)mutex);
macro bool Mutex.try_lock(&mutex) => NativeMutex.try_lock((NativeMutex*)mutex);
macro void? Mutex.unlock(&mutex) @maydiscard => NativeMutex.unlock((NativeMutex*)mutex); // Remove optional in 0.8.0

macro void? TimedMutex.init(&mutex) => NativeTimedMutex.init((NativeTimedMutex*)mutex, {.timed});
macro void? TimedRecursiveMutex.init(&mutex) @maydiscard => NativeTimedMutex.init((NativeTimedMutex*)mutex, {.timed, .recursive});
macro void? TimedMutex.destroy(&mutex) @maydiscard => NativeTimedMutex.destroy((NativeTimedMutex*)mutex); // Remove optional in 0.8.0
macro void? TimedMutex.lock(&mutex) @maydiscard => NativeTimedMutex.lock((NativeTimedMutex*)mutex);
macro void? TimedMutex.lock_timeout(&mutex, ulong ms) => NativeTimedMutex.lock_timeout((NativeTimedMutex*)mutex, ms);
macro bool  TimedMutex.try_lock(&mutex) => NativeTimedMutex.try_lock((NativeTimedMutex*)mutex);
macro void? TimedMutex.unlock(&mutex) @maydiscard => NativeTimedMutex.unlock((NativeTimedMutex*)mutex); // Remove optional in 0.8.0

macro void fence(AtomicOrdering $ordering) @safemacro
{
	$$fence($ordering.ordinal);
}

macro void Mutex.@in_lock(&mutex; @body)
{
	mutex.lock();
	defer mutex.unlock();
	@body();
}

macro void? ConditionVariable.init(&cond) => NativeConditionVariable.init((NativeConditionVariable*)cond);
macro void? ConditionVariable.destroy(&cond) @maydiscard => NativeConditionVariable.destroy((NativeConditionVariable*)cond);
macro void? ConditionVariable.signal(&cond) @maydiscard => NativeConditionVariable.signal((NativeConditionVariable*)cond);
macro void? ConditionVariable.broadcast(&cond) @maydiscard => NativeConditionVariable.broadcast((NativeConditionVariable*)cond);
macro void? ConditionVariable.wait(&cond, Mutex* mutex) @maydiscard
{
	return NativeConditionVariable.wait((NativeConditionVariable*)cond, (NativeMutex*)mutex);
}
<*
 @require $defined(Duration d = #ms_or_duration) ||| $defined(ulong l = #ms_or_duration)
 @return? thread::WAIT_TIMEOUT
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
 @return? thread::WAIT_TIMEOUT
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

macro void? Thread.detach(thread) @maydiscard => NativeThread.detach(thread);

<*
	@return? THREAD_NOT_FOUND : "If the thread cannot be found."
*>
macro int? Thread.join(thread) @maydiscard => NativeThread.join(thread);
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
