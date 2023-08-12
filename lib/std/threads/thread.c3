module std::thread;
import std::thread::os;

def MutexType = distinct int;

const MutexType MUTEX_PLAIN = 0;
const MutexType MUTEX_TIMED = 1;
const MutexType MUTEX_RECURSIVE = 2;

def Mutex = distinct NativeMutex;
def ConditionVariable = distinct NativeConditionVariable;
def Thread = distinct NativeThread;
def OnceFlag = distinct NativeOnceFlag;
def OnceFn = fn void();

def ThreadFn = fn int(void* arg);

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

macro void! Mutex.init(&mutex, MutexType type = MUTEX_PLAIN) => NativeMutex.init((NativeMutex*)mutex, type);
macro void! Mutex.destroy(&mutex) => NativeMutex.destroy((NativeMutex*)mutex);
macro void! Mutex.lock(&mutex) => NativeMutex.lock((NativeMutex*)mutex);
macro void! Mutex.lock_timeout(&mutex, ulong ms) => NativeMutex.lock_timeout((NativeMutex*)mutex, ms);
macro bool Mutex.try_lock(&mutex) => NativeMutex.try_lock((NativeMutex*)mutex);
macro void! Mutex.unlock(&mutex) => NativeMutex.unlock((NativeMutex*)mutex);

macro void! ConditionVariable.init(&cond) => NativeConditionVariable.init((NativeConditionVariable*)cond);
macro void! ConditionVariable.destroy(&cond) => NativeConditionVariable.destroy((NativeConditionVariable*)cond);
macro void! ConditionVariable.signal(&cond) => NativeConditionVariable.signal((NativeConditionVariable*)cond);
macro void! ConditionVariable.broadcast(&cond) => NativeConditionVariable.broadcast((NativeConditionVariable*)cond);
macro void! ConditionVariable.wait(&cond, Mutex* mutex)
{
	return NativeConditionVariable.wait((NativeConditionVariable*)cond, (NativeMutex*)mutex);
}
macro void! ConditionVariable.wait_timeout(&cond, Mutex* mutex, ulong timeout)
{
	return NativeConditionVariable.wait_timeout((NativeConditionVariable*)cond, (NativeMutex*)mutex, timeout);
}


macro void! Thread.create(&thread, ThreadFn thread_fn, void* arg) => NativeThread.create((NativeThread*)thread, thread_fn, arg);
macro void! Thread.detach(thread) => NativeThread.detach((NativeThread)thread);
macro int! Thread.join(thread) => NativeThread.join((NativeThread)thread);
macro bool Thread.equals(thread, Thread other) => NativeThread.equals((NativeThread)thread, (NativeThread)other);

macro void OnceFlag.call(&flag, OnceFn func) => NativeOnceFlag.call_once((NativeOnceFlag*)flag, func);

macro void yield() => os::native_thread_yield();
macro Thread current() => os::native_thread_current();
macro void exit(int result) => os::native_thread_exit(result);
macro void! sleep(double s) @maydiscard => os::native_sleep(s);
macro void! sleep_ms(ulong ms) @maydiscard  => os::native_sleep_ms(ms);
macro void! sleep_ns(ulong ns) @maydiscard  => os::native_sleep_nano(ns);

