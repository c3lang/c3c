module std::thread::os @if (!env::POSIX && !env::WIN32);

typedef NativeMutex = int;
typedef NativeTimedMutex = int;
typedef NativeConditionVariable = int;
typedef NativeOnceFlag = int;
typedef NativeThread = int;

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	if (*flag == 0)
	{
		*flag = 1;
		func();
	}
}

fn void? NativeMutex.init(&mtx, MutexType type) => NOT_IMPLEMENTED?;

fn bool NativeMutex.is_initialized(&self)
{
	return false;
}

macro void NativeMutex.lock(&mutex) => NOT_IMPLEMENTED?!!;
macro bool NativeMutex.try_lock(&mutex) => NOT_IMPLEMENTED?!!;
macro void NativeMutex.unlock(&mutex) => NOT_IMPLEMENTED?!!;