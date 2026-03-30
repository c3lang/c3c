module std::thread::os @if (!env::NATIVE_THREADING);
import libc;

typedef NativeMutex = int;
typedef NativeTimedMutex = int;
typedef NativeConditionVariable = int;
typedef NativeOnceFlag = int;
typedef NativeThread = int;

fn NativeThread native_thread_current()
{
	return {};
}

fn void native_thread_exit(int result)
{
	$if env::NO_LIBC:
		$$trap();
	$else
		libc::exit(result);
	$endif
}

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	if (*flag == (NativeOnceFlag)0)
	{
		*flag = (NativeOnceFlag)1;
		func();
	}
}

fn bool NativeThread.equals(thread, NativeThread other) => true;

fn void? NativeMutex.init(&mtx, MutexType type) => NOT_IMPLEMENTED~;

fn bool NativeMutex.is_initialized(&self)
{
	return false;
}

macro void NativeMutex.lock(&mutex) => NOT_IMPLEMENTED~!!;
macro bool NativeMutex.try_lock(&mutex) => NOT_IMPLEMENTED~!!;
macro void NativeMutex.unlock(&mutex) => NOT_IMPLEMENTED~!!;