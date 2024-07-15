module std::core::sanitizer::tsan;

distinct Mutex_Flags = inline CUInt;

const Mutex_Flags MUTEX_LINKER_INIT      = 1 << 0;
const Mutex_Flags MUTEX_WRITE_REENTRANT  = 1 << 1;
const Mutex_Flags MUTEX_READ_REENTRANT   = 1 << 2;
const Mutex_Flags MUTEX_NOT_STATIC       = 1 << 8;
const Mutex_Flags MUTEX_READ_LOCK        = 1 << 3;
const Mutex_Flags MUTEX_TRY_LOCK         = 1 << 4;
const Mutex_Flags MUTEX_TRY_LOCK_FAILED  = 1 << 5;
const Mutex_Flags MUTEX_RECURSIVE_LOCK   = 1 << 6;
const Mutex_Flags MUTEX_RECURSIVE_UNLOCK = 1 << 7;
const Mutex_Flags MUTEX_TRY_READ_LOCK = MUTEX_READ_LOCK | MUTEX_TRY_LOCK;
const Mutex_Flags MUTEX_TRY_READ_LOCK_FAILED = MUTEX_TRY_READ_LOCK | MUTEX_TRY_LOCK_FAILED;

macro void mutex_create(void* addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_create(addr, flags); $endif }
macro void mutex_destroy(void* addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_destroy(addr, flags); $endif }
macro void mutex_pre_lock(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_lock(addr, flags); $endif }
macro void mutex_post_lock(void *addr, Mutex_Flags flags, CInt recursion) { $if env::THREAD_SANITIZER: __tsan_mutex_post_lock(addr, flags, recursion); $endif }
macro CInt mutex_pre_unlock(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: return __tsan_mutex_pre_unlock(addr, flags); $else return 0; $endif }
macro void mutex_post_unlock(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_unlock(addr, flags); $endif }
macro void mutex_pre_signal(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_signal(addr, flags); $endif }
macro void mutex_post_signal(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_signal(addr, flags); $endif }
macro void mutex_pre_divert(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_pre_divert(addr, flags); $endif }
macro void mutex_post_divert(void *addr, Mutex_Flags flags) { $if env::THREAD_SANITIZER: __tsan_mutex_post_divert(addr, flags); $endif }

module std::core::sanitizer::tsan @if(env::THREAD_SANITIZER) @private;

extern fn void __tsan_mutex_create(void *addr, CUInt flags);
extern fn void __tsan_mutex_destroy(void *addr, CUInt flags);
extern fn void __tsan_mutex_pre_lock(void *addr, CUInt flags);
extern fn void __tsan_mutex_post_lock(void *addr, CUInt flags, CInt recursion);
extern fn CInt __tsan_mutex_pre_unlock(void *addr, CUInt flags);
extern fn void __tsan_mutex_post_unlock(void *addr, CUInt flags);
extern fn void __tsan_mutex_pre_signal(void *addr, CUInt flags);
extern fn void __tsan_mutex_post_signal(void *addr, CUInt flags);
extern fn void __tsan_mutex_pre_divert(void *addr, CUInt flags);
extern fn void __tsan_mutex_post_divert(void *addr, CUInt flags);
