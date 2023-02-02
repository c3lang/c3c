module std::thread::os;

$if (thread::THREAD_MODEL == ThreadModel.WIN32):

define NativeThread = Win32_HANDLE;
define Win32_CRITICAL_SECTION = distinct ulong[5];
define Win32_HANDLE = distinct ulong;

struct NativeMutex
{
	union
	{
        Win32_CRITICAL_SECTION critical_section;
        Win32_HANDLE handle;
    }
    bool already_locked;
    bool recursive;
    bool timed;
}

struct NativeOnceFlag
{
	int status;
	Win32_CRITICAL_SECTION lock;
}

struct NativeConditionVariable
{
	union
	{
		struct
		{
			Win32_HANDLE event_one;
			Win32_HANDLE event_all;
		}
		Win32_HANDLE[2] events;
	}
	uint waiters_count;
	Win32_CRITICAL_SECTION waiters_count_lock;
}

extern fn void win32_InitializeCriticalSection(Win32_CRITICAL_SECTION* section) @extern("InitializeCriticalSection");
extern fn void win32_DeleteCriticalSection(Win32_CRITICAL_SECTION* section) @extern("DeleteCriticalSection");
extern fn Win32_HANDLE win32_CreateMutex(void*, bool, void*) @extern("CreateMutexA");
extern fn bool win32_CloseHandle(Win32_HANDLE) @extern("CloseHandle");
extern fn bool win32_ReleaseMutex(Win32_HANDLE) @extern("ReleaseMutex");
extern fn void win32_EnterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("EnterCriticalSection");
extern fn void win32_LeaveCriticalSection(Win32_CRITICAL_SECTION* section) @extern("LeaveCriticalSection");
extern fn bool win32_TryEnterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("TryEnterCriticalSection");
extern fn uint win32_WaitForSingleObject(Win32_HANDLE, uint milliseconds) @extern("WaitForSingleObject");
extern fn void win32_Sleep(uint ms) @extern("Sleep");
extern fn uint win32_WaitForMultipleObjects(uint count, Win32_HANDLE* handles, bool wait_all, uint ms) @extern("WaitForMultipleObjects");
extern fn Win32_HANDLE win32_CreateEventA(void* attributes, bool manual_reset, bool initial_state, char* name) @extern("CreateEventA");
extern fn bool win32_ResetEvent(Win32_HANDLE event) @extern("ResetEvent");
extern fn bool win32_SetEvent(Win32_HANDLE handle) @extern("SetEvent");
extern fn long win32_InterlockedCompareExchange(int* dest, int exchange, int comperand) @extern("InterlockedCompareExchange");
extern fn uint win32_SleepEx(uint ms, bool alertable) @extern("SleepEx");
extern fn Win32_HANDLE win32_CreateThread(void* attributes, usz stack, ThreadFn func, void* arg, uint flags, uint* thread_id) @extern("CreateThread");
extern fn bool win32_GetExitCodeThread(Win32_HANDLE handle, uint* exit_code) @extern("GetExitCodeThread");
extern fn uint win32_GetThreadId(Win32_HANDLE) @extern("GetThreadId");
extern fn void win32_ExitThread(uint res) @noreturn @extern("ExitThread");
extern fn Win32_HANDLE win32_GetCurrentThread() @extern("GetCurrentThread");

const uint WAIT_OBJECT_0 = 0;
const uint WAIT_ABANDONED = 128;
const uint WAIT_TIMEOUT = 258;
const uint WAIT_IO_COMPLETION = 192;
const uint WAIT_FAILED = (uint)-1;
const uint INFINITE = (uint)-1;

fn void! NativeMutex.init(NativeMutex* mtx, MutexType type)
{
	mtx.already_locked = false;
    mtx.recursive = (bool)(type & thread::MUTEX_RECURSIVE);
    mtx.timed = (bool)(type & thread::MUTEX_TIMED);
    if (!mtx.timed)
    {
        win32_InitializeCriticalSection(&(mtx.critical_section));
        return;
    }
    if (!(mtx.handle = win32_CreateMutex(null, false, null))) return ThreadFault.INIT_FAILED!;
}

fn void! NativeMutex.destroy(NativeMutex* mtx)
{
	if (!mtx.timed)
	{
		win32_DeleteCriticalSection(&mtx.critical_section);
		return;
	}
    if (!win32_CloseHandle(mtx.handle)) return ThreadFault.DESTROY_FAILED!;
}

fn void! NativeMutex.lock(NativeMutex* mtx)
{
	if (!mtx.timed)
    {
        win32_EnterCriticalSection(&mtx.critical_section);
    }
    else
    {
        switch (win32_WaitForSingleObject(mtx.handle, INFINITE))
        {
            case WAIT_OBJECT_0:
                break;
			case WAIT_ABANDONED:
			default:
				return ThreadFault.LOCK_FAILED!;

        }
    }
	if (!mtx.recursive)
	{
		while (mtx.already_locked) win32_Sleep(1);
	}
	mtx.already_locked = true;
}


/**
 * @require mtx.timed "Only available for timed locks"
 **/
fn void! NativeMutex.lock_timeout(NativeMutex* mtx, uint ms)
{
	switch (win32_WaitForSingleObject(mtx.handle, ms))
	{
		case WAIT_OBJECT_0:
			break;
		case WAIT_TIMEOUT:
			return ThreadFault.LOCK_TIMEOUT!;
		case WAIT_ABANDONED:
		default:
			return ThreadFault.LOCK_FAILED!;
	}
	if (!mtx.recursive)
	{
		while (mtx.already_locked) win32_Sleep(1);
		mtx.already_locked = true;
	}
}

fn bool NativeMutex.try_lock(NativeMutex* mtx)
{
	bool success = mtx.timed
		? win32_WaitForSingleObject(mtx.handle, 0) == WAIT_OBJECT_0
		: win32_TryEnterCriticalSection(&mtx.critical_section);

	if (!success) return false;
	if (!mtx.recursive)
	{
		if (mtx.already_locked)
		{
			assert(!mtx.timed);
			win32_LeaveCriticalSection(&mtx.critical_section);
			return false;
		}
		mtx.already_locked = true;
	}
	return true;
}

fn void! NativeMutex.unlock(NativeMutex* mtx)
{
	mtx.already_locked = false;
	if (!mtx.timed)
	{
	    win32_LeaveCriticalSection(&mtx.critical_section);
	    return;
	}
    if (!win32_ReleaseMutex(mtx.handle)) return ThreadFault.UNLOCK_FAILED!;
}

const int CONDITION_EVENT_ONE = 0;
const int CONDITION_EVENT_ALL = 1;

fn void! NativeConditionVariable.init(NativeConditionVariable* cond)
{
	cond.waiters_count = 0;
	win32_InitializeCriticalSection(&cond.waiters_count_lock);
	cond.event_one = win32_CreateEventA(null, false, false, null);
	if (!cond.event_one)
	{
		cond.event_all = (Win32_HANDLE)0;
		return ThreadFault.INIT_FAILED!;
	}
	cond.event_all = win32_CreateEventA(null, true, false, null);
	if (!cond.event_all)
	{
		win32_CloseHandle(cond.event_one);
		cond.event_one = (Win32_HANDLE)0;
		return ThreadFault.INIT_FAILED!;
	}
}

fn void! NativeConditionVariable.destroy(NativeConditionVariable* cond) @maydiscard
{
	if (cond.event_one) win32_CloseHandle(cond.event_one);
	if (cond.event_all) win32_CloseHandle(cond.event_all);
	win32_DeleteCriticalSection(&cond.waiters_count_lock);
}

fn void! NativeConditionVariable.signal(NativeConditionVariable* cond)
{
	win32_EnterCriticalSection(&cond.waiters_count_lock);
	bool have_waiters = cond.waiters_count > 0;
	win32_LeaveCriticalSection(&cond.waiters_count_lock);
	if (have_waiters && !win32_SetEvent(cond.event_one)) return ThreadFault.SIGNAL_FAILED!;
}

fn void! NativeConditionVariable.broadcast(NativeConditionVariable* cond)
{
	win32_EnterCriticalSection(&cond.waiters_count_lock);
	bool have_waiters = cond.waiters_count > 0;
	win32_LeaveCriticalSection(&cond.waiters_count_lock);
	if (have_waiters && !win32_SetEvent(cond.event_all)) return ThreadFault.SIGNAL_FAILED!;
}

private fn void! timedwait(NativeConditionVariable* cond, NativeMutex* mtx, uint timeout)
{
	win32_EnterCriticalSection(&cond.waiters_count_lock);
    cond.waiters_count++;
	win32_LeaveCriticalSection(&cond.waiters_count_lock);

	mtx.unlock()?;

    uint result = win32_WaitForMultipleObjects(2, &cond.events, false, timeout);
    switch (result)
    {
        case WAIT_TIMEOUT:
            mtx.lock()?;
            return ThreadFault.WAIT_TIMEOUT!;
        case WAIT_FAILED:
            mtx.lock()?;
            return ThreadFault.WAIT_FAILED!;
        default:
            break;
    }

	win32_EnterCriticalSection(&cond.waiters_count_lock);
	cond.waiters_count--;
	// If event all && no waiters
	bool last_waiter = result == 1 && !cond.waiters_count;
	win32_LeaveCriticalSection(&cond.waiters_count_lock);

	if (last_waiter)
	{
		if (!win32_ResetEvent(cond.event_all))
		{
			mtx.lock()?;
			return ThreadFault.WAIT_FAILED!;
		}
	}

	mtx.lock()?;
}

fn void! NativeConditionVariable.wait(NativeConditionVariable* cond, NativeMutex* mtx) @inline
{
	return timedwait(cond, mtx, INFINITE) @inline;
}

fn void! NativeConditionVariable.wait_timeout(NativeConditionVariable* cond, NativeMutex* mtx, uint time) @inline
{
	return timedwait(cond, mtx, time) @inline;
}

fn void! NativeThread.create(NativeThread* thread, ThreadFn func, void* args)
{
	if (!(*thread = win32_CreateThread(null, 0, func, args, 0, null))) return ThreadFault.INIT_FAILED!;
}

fn void! NativeThread.detach(NativeThread thread) @inline
{
	if (!win32_CloseHandle(thread)) return ThreadFault.DETACH_FAILED!;
}


fn void native_thread_exit(int result) @inline
{
	win32_ExitThread((uint)result);
}

fn void native_thread_yield()
{
	win32_Sleep(0);
}

fn void NativeOnceFlag.call_once(NativeOnceFlag* flag, OnceFn func)
{
    while (@volatile_load(flag.status) < 3)
    {
        switch (@volatile_load(flag.status))
        {
            case 0:
                if (mem::compare_exchange_volatile(&flag.status, 1, 0, AtomicOrdering.SEQ_CONSISTENT, AtomicOrdering.SEQ_CONSISTENT) == 0)
                {
                    win32_InitializeCriticalSection(&flag.lock);
                    win32_EnterCriticalSection(&flag.lock);
                    @volatile_store(flag.status, 2);
                    func();
                    @volatile_store(flag.status, 3);
                    win32_LeaveCriticalSection(&flag.lock);
                    return;
                }
                break;
            case 1:
                break;
            case 2:
                win32_EnterCriticalSection(&flag.lock);
                win32_LeaveCriticalSection(&flag.lock);
                break;
        }
    }
}

fn void! NativeThread.join(NativeThread thread, int *res)
{
	if (win32_WaitForSingleObject(thread, INFINITE) == WAIT_FAILED) return ThreadFault.JOIN_FAILED!;
	if (!win32_GetExitCodeThread(thread, (uint*)res)) return ThreadFault.JOIN_FAILED!;
	defer win32_CloseHandle(thread);
}

fn NativeThread native_thread_current()
{
	return win32_GetCurrentThread();
}

fn bool NativeThread.equals(NativeThread this, NativeThread other)
{
	return win32_GetThreadId(this) == win32_GetThreadId(other);
}

/**
 * @require ms < uint.max "Too long sleep"
 **/
fn void! native_sleep_ms(ulong ms)
{
	if (win32_SleepEx((uint)ms, true) == WAIT_IO_COMPLETION) return ThreadFault.INTERRUPTED!;
}

fn void! native_sleep(double s)
{
	return native_sleep_ms((uint)s * 1000);
}

fn void! native_sleep_nano(ulong ns)
{
	return native_sleep_ms(ns < 1000_000 ? 1 : ns / 1000_000);
}

$endif;