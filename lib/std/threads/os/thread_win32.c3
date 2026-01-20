module std::thread::os @if(env::WIN32);
import std::os::win32, std::time;
import std::thread;


typedef NativeThread = inline Win32_HANDLE;

struct NativeMutex
{
	Win32_SRWLOCK srw_lock;
	Win32_DWORD owner_thread;
	bitstruct : uint
	{
		bool initialized : 0;
		bool recursive   : 1;
		uint locks       : 2..31;
	}
}

fn bool NativeMutex.is_initialized(&self)
{
	return self.initialized;
}

struct NativeTimedMutex
{
	Win32_SRWLOCK srw_lock;
	Win32_CONDITION_VARIABLE cond_var;
	Win32_DWORD owner_thread;
	bitstruct : uint
	{
		bool initialized : 0;
		bool recursive   : 1;
		uint locks       : 2..31;
	}
}

struct NativeConditionVariable
{
	Win32_CONDITION_VARIABLE cond_var;
}

struct NativeOnceFlag
{
	Win32_INIT_ONCE init_once;
}

<*
 @require !mtx.initialized : "Mutex is already initialized"
 @require !type.timed
 @ensure mtx.initialized
*>
fn void? NativeMutex.init(&mtx, MutexType type)
{
	*mtx = {
		.initialized = true,
		.recursive = type.recursive,
	};
}

<*
 @require mtx.initialized : "Mutex was not initialized"
 @require mtx.owner_thread != win32::getCurrentThreadId() : "Mutex was not unlocked before destroying it"
 @ensure !mtx.initialized
*>
fn void NativeMutex.destroy(&mtx)
{
	*mtx = {};
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn void NativeMutex.lock(&mtx)
{
	Win32_DWORD current_thread = win32::getCurrentThreadId();
	if (mtx.owner_thread == current_thread)
	{
		assert(mtx.recursive, "Recursive locking with non-recursive mutex");
		mtx.locks++;
		return;
	}

	win32::acquireSRWLockExclusive(&mtx.srw_lock);
	mtx.owner_thread = current_thread;
	mtx.locks = 1;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn bool NativeMutex.try_lock(&mtx)
{
	Win32_DWORD current_thread = win32::getCurrentThreadId();
	if (mtx.owner_thread == current_thread)
	{
		if (!mtx.recursive) return false;
		mtx.locks++;
		return true;
	}

	if (!win32::tryAcquireSRWLockExclusive(&mtx.srw_lock)) return false;

	mtx.owner_thread = current_thread;
	mtx.locks = 1;
	return true;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
 @require mtx.owner_thread == win32::getCurrentThreadId() : "Mutex was not locked by the current thread"
*>
fn void NativeMutex.unlock(&mtx)
{
	if (--mtx.locks == 0)
	{
		mtx.owner_thread = 0;
		win32::releaseSRWLockExclusive(&mtx.srw_lock);
	}
}

<*
 @require type.timed
 @require !mtx.initialized : "Mutex is already initialized"
 @ensure mtx.initialized
*>
fn void? NativeTimedMutex.init(&mtx, MutexType type)
{
	*mtx = {
		.initialized = true,
		.recursive = type.recursive,
	};
}

<*
 @require mtx.initialized : "Mutex was not initialized"
 @require mtx.owner_thread != win32::getCurrentThreadId() : "Mutex was not unlocked before destroying it"
 @ensure !mtx.initialized
*>
fn void NativeTimedMutex.destroy(&mtx)
{
	*mtx = {};
}

fn void? NativeTimedMutex.wait_cond_var(&mtx, uint ms) @local
{
	if (!win32::sleepConditionVariableSRW(&mtx.cond_var, &mtx.srw_lock, ms, 0))
	{
		Win32_DWORD err = win32::getLastError();
		assert(err == win32::ERROR_TIMEOUT, "Unexpected error waiting for condvar %d", err);
	}
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn void NativeTimedMutex.lock(&mtx)
{
	Win32_DWORD current_thread = win32::getCurrentThreadId();
	if (mtx.owner_thread == current_thread)
	{
		assert(mtx.recursive, "Tried to lock a non-recursive mutex");
		mtx.locks++;
		return;
	}

	win32::acquireSRWLockExclusive(&mtx.srw_lock);
	defer win32::releaseSRWLockExclusive(&mtx.srw_lock);

	while (mtx.locks)
	{
		mtx.wait_cond_var(win32::INFINITE)!!;
	}
	mtx.locks = 1;
	mtx.owner_thread = current_thread;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
 @return? thread::WAIT_TIMEOUT
*>
fn void? NativeTimedMutex.lock_timeout(&mtx, ulong ms)
{
	Win32_DWORD current_thread = win32::getCurrentThreadId();
	if (mtx.owner_thread == current_thread)
	{
		assert(mtx.recursive, "The mutex was not recursive, but was locked in the same thread more than once.");
		mtx.locks++;
		return;
	}

	win32::acquireSRWLockExclusive(&mtx.srw_lock);
	defer win32::releaseSRWLockExclusive(&mtx.srw_lock);

	if (!mtx.locks)
	{
		// Got the lock without needing to wait
		mtx.locks = 1;
		mtx.owner_thread = current_thread;
		return;
	}

	NanoDuration duration = time::ms(ms).to_nano();
	Clock start = clock::now();
	for (NanoDuration remaining = duration; remaining > time::NANO_DURATION_ZERO; remaining = duration - start.to_now())
	{
		ulong remaining_ms = remaining.to_ms();
		if (remaining_ms > uint.max) remaining_ms = uint.max;
		mtx.wait_cond_var((uint)remaining_ms)!;
		if (!mtx.locks)
		{
			// Got the lock
			mtx.locks = 1;
			mtx.owner_thread = current_thread;
			return;
		}
	}
	return thread::WAIT_TIMEOUT~;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn bool NativeTimedMutex.try_lock(&mtx)
{
	Win32_DWORD current_thread = win32::getCurrentThreadId();
	if (mtx.owner_thread == current_thread)
	{
		if (!mtx.recursive) return false;
		mtx.locks++;
		return true;
	}

	win32::acquireSRWLockExclusive(&mtx.srw_lock);
	defer win32::releaseSRWLockExclusive(&mtx.srw_lock);

	if (mtx.locks) return false;
	mtx.locks = 1;
	mtx.owner_thread = current_thread;
	return true;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn void NativeTimedMutex.unlock(&mtx)
{
	win32::acquireSRWLockExclusive(&mtx.srw_lock);
	$if env::COMPILER_SAFE_MODE:
		if (mtx.owner_thread != win32::getCurrentThreadId()) 
		{
			win32::releaseSRWLockExclusive(&mtx.srw_lock);
			abort("Mutex was not locked by the current thread");
		}
	$endif

	bool signal;
	if (--mtx.locks == 0)
	{
		mtx.owner_thread = 0;
		signal = true;
	}
	win32::releaseSRWLockExclusive(&mtx.srw_lock);
	if (signal) win32::wakeConditionVariable(&mtx.cond_var);
}

fn void? NativeConditionVariable.init(&cond)
{
	cond.cond_var = {};
}

fn void NativeConditionVariable.destroy(&cond)
{
	// Nothing to do
}

fn void NativeConditionVariable.signal(&cond)
{
	win32::wakeConditionVariable(&cond.cond_var);
}

fn void NativeConditionVariable.broadcast(&cond)
{
	win32::wakeAllConditionVariable(&cond.cond_var);
}

<*
 @return "true if wait succeeded, false if it timed out"
*>
fn bool timedwait(NativeConditionVariable* cond, NativeMutex* mtx, uint timeout) @private
{
	Win32_DWORD owner_thread = mtx.owner_thread;
	assert(mtx.locks == 1, "Expected lock count = 1, was %d", mtx.locks);
	assert(owner_thread == win32::getCurrentThreadId(), "Timed wait on other thread");
	mtx.owner_thread = 0;
	mtx.locks = 0;
	defer
	{
		mtx.owner_thread = owner_thread;
		mtx.locks = 1;
	}
	if (!win32::sleepConditionVariableSRW(&cond.cond_var, &mtx.srw_lock, timeout, 0))
	{
		if (win32::getLastError() == win32::ERROR_TIMEOUT)
		{
			return false;
		}
		abort("Unexpected error sleepConditionVariableSRW: %d", win32::getLastError());
	}
	return true;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
*>
fn void NativeConditionVariable.wait(&cond, NativeMutex* mtx) @inline
{
	bool res = timedwait(cond, mtx, win32::INFINITE) @inline;
	assert(res, "Timed wait exited despite inifite time");
}

<*
 @require mtx.initialized : "Mutex was not initialized"
  @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_timeout(&cond, NativeMutex* mtx, ulong ms) @inline
{
	if (ms > uint.max) ms = uint.max;
	if (!timedwait(cond, mtx, (uint)ms) @inline) return thread::WAIT_TIMEOUT~;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
  @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_timeout_duration(&cond, NativeMutex* mtx, Duration duration) @inline
{
	if (duration < time::DURATION_ZERO) return thread::WAIT_TIMEOUT~;
	long ms = duration.to_ms();
	if (ms > uint.max) ms = uint.max;
	if (!timedwait(cond, mtx, (uint)ms) @inline) return thread::WAIT_TIMEOUT~;
}

<*
 @require mtx.initialized : "Mutex was not initialized"
 @return? thread::WAIT_TIMEOUT
*>
fn void? NativeConditionVariable.wait_until(&cond, NativeMutex* mtx, Time time) @inline
{
	Duration duration = time - time::now();
	return cond.wait_timeout_duration(mtx, duration);
}

fn void? NativeThread.create(&thread, ThreadFn func, void* args)
{
	if (!(*thread = (NativeThread)win32::createThread(null, 0, func, args, 0, null))) return thread::INIT_FAILED~;
}

fn void NativeThread.detach(thread) @inline
{
	if (!win32::closeHandle(thread)) abort("Detaching thread failed");
}



fn void native_thread_exit(int result) @inline
{
	win32::exitThread((uint)result);
}

fn void native_thread_yield()
{
	win32::sleep(0);
}

fn void NativeOnceFlag.call_once(&flag, OnceFn func)
{
	var callback = fn Win32_BOOL(Win32_INIT_ONCE* init_once, void* parameter, void** context)
	{
		((OnceFn)parameter)();
		return 1;
	};
	win32::initOnceExecuteOnce(&flag.init_once, callback, func, null);
}

<*
 @return "the exit value of the thread"
*>
fn int NativeThread.join(thread)
{
	uint res;
	if (win32::waitForSingleObject(thread, win32::INFINITE) == win32::WAIT_FAILED) unreachable("Failed to join thread, received wait failed.");
	if (!win32::getExitCodeThread(thread, &res)) unreachable("Failed to retrieve exit code when joining.");
	defer win32::closeHandle(thread);
	return res;
}

fn NativeThread native_thread_current()
{
	return (NativeThread)win32::getCurrentThread();
}

fn bool NativeThread.equals(thread, NativeThread other)
{
	return win32::getThreadId(thread) == win32::getThreadId(other);
}

fn void? native_sleep_nano(NanoDuration ns)
{
	long ms = ns.to_ms();
	if (ms <= 0) return;
	if (ms > Win32_DWORD.max) ms = Win32_DWORD.max;
	if (win32::sleepEx((Win32_DWORD)ms, (Win32_BOOL)true) == win32::WAIT_IO_COMPLETION) return thread::INTERRUPTED~;
}
