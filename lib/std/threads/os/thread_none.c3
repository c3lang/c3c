module std::thread::os @feat(!POSIX & !WIN32);
import libc, std::time;

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

fn void? NativeConditionVariable.init(&self) => NOT_IMPLEMENTED~;
fn void NativeConditionVariable.destroy(&self) {}
fn void NativeConditionVariable.signal(&self) {}
fn void NativeConditionVariable.broadcast(&self) {}
fn void? NativeConditionVariable.wait_until(&self, NativeMutex* mutex, Time time) {}
fn void? NativeConditionVariable.wait(&self, NativeMutex* mutex) {}
fn int? NativeThread.join(self) => 0;
fn void? NativeThread.create(&self, ThreadFn thread_fn, void* arg, ThreadSettings settings) => NOT_IMPLEMENTED~;

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

fn void NativeMutex.lock(&mutex) => NOT_IMPLEMENTED~!!;
fn bool NativeMutex.try_lock(&mutex) => NOT_IMPLEMENTED~!!;
fn void NativeMutex.unlock(&mutex) => NOT_IMPLEMENTED~!!;
fn void NativeMutex.destroy(&mutex) => NOT_IMPLEMENTED~!!;