module std::os::posix @if(env::POSIX);
import std::thread;
import libc;

const PTHREAD_MUTEX_NORMAL = 0;
const PTHREAD_MUTEX_ERRORCHECK = env::LINUX ? 2 : 1;
const PTHREAD_MUTEX_RECURSIVE = env::LINUX ? 1 : 2;

alias PosixThreadFn = fn void*(void*);
typedef Pthread_t = void*;

extern fn CInt pthread_create(Pthread_t*, Pthread_attr_t*, PosixThreadFn, void*);
extern fn CInt pthread_cancel(Pthread_t*);
extern fn CInt pthread_detach(Pthread_t);
extern fn CInt pthread_equal(Pthread_t this, Pthread_t other);
extern fn void pthread_exit(void* value_ptr) @noreturn;
extern fn CInt pthread_join(Pthread_t, void** value_ptr);
extern fn CInt pthread_kill(Pthread_t, CInt sig);
extern fn void pthread_once(Pthread_once_t*, OnceFn);
extern fn Pthread_t pthread_self();
extern fn CInt pthread_setcancelstate(CInt state, CInt* oldstate);
extern fn CInt pthread_setcanceltype(CInt type, CInt* oldtype);
extern fn CInt pthread_testcancel();

extern fn CInt pthread_attr_destroy(Pthread_attr_t*);
extern fn CInt pthread_attr_getinheritsched(Pthread_attr_t*, CInt*);
extern fn CInt pthread_attr_getschedparam(Pthread_attr_t*, Pthread_sched_param*);
extern fn CInt pthread_attr_getschedpolicy(Pthread_attr_t*, CInt*);
extern fn CInt pthread_attr_getscope(Pthread_attr_t*, CInt*);
extern fn CInt pthread_attr_getstacksize(Pthread_attr_t*, usz*);
extern fn CInt pthread_attr_getstackaddr(Pthread_attr_t*, void**);
extern fn CInt pthread_attr_getdetachstate(Pthread_attr_t*, CInt*);
extern fn CInt pthread_attr_init(Pthread_attr_t*);
extern fn CInt pthread_attr_setinheritsched(Pthread_attr_t*, CInt);
extern fn CInt pthread_attr_setschedparam(Pthread_attr_t*, Pthread_sched_param*);
extern fn CInt pthread_attr_setschedpolicy(Pthread_attr_t*, CInt);
extern fn CInt pthread_attr_setscope(Pthread_attr_t*, CInt);
extern fn CInt pthread_attr_setstacksize(Pthread_attr_t*, usz);
extern fn CInt pthread_attr_setstackaddr(Pthread_attr_t*, void*);
extern fn CInt pthread_attr_setdetachstate(Pthread_attr_t*, CInt);

extern fn CInt pthread_mutexattr_destroy(Pthread_mutexattr_t*);
extern fn CInt pthread_mutexattr_getprioceiling(Pthread_mutexattr_t*, CInt*);
extern fn CInt pthread_mutexattr_getprotocol(Pthread_mutexattr_t*, CInt*);
extern fn CInt pthread_mutexattr_gettype(Pthread_mutexattr_t*, CInt*);
extern fn CInt pthread_mutexattr_init(Pthread_mutexattr_t*);
extern fn CInt pthread_mutexattr_setprioceiling(Pthread_mutexattr_t*, CInt);
extern fn CInt pthread_mutexattr_setprotocol(Pthread_mutexattr_t*, CInt);
extern fn CInt pthread_mutexattr_settype(Pthread_mutexattr_t*, CInt);

extern fn CInt pthread_mutex_destroy(Pthread_mutex_t*);
extern fn CInt pthread_mutex_init(Pthread_mutex_t*, Pthread_mutexattr_t*);
extern fn Errno pthread_mutex_lock(Pthread_mutex_t*);
extern fn Errno pthread_mutex_trylock(Pthread_mutex_t*);
extern fn Errno pthread_mutex_unlock(Pthread_mutex_t*);

extern fn CInt pthread_condattr_destroy(Pthread_condattr_t*);
extern fn CInt pthread_condattr_init(Pthread_condattr_t*);

extern fn CInt pthread_cond_broadcast(Pthread_cond_t*);
extern fn CInt pthread_cond_destroy(Pthread_cond_t*);
extern fn CInt pthread_cond_init(Pthread_cond_t*, Pthread_condattr_t*);
extern fn CInt pthread_cond_signal(Pthread_cond_t*);
extern fn CInt pthread_cond_timedwait(Pthread_cond_t*, Pthread_mutex_t*, TimeSpec*);
extern fn CInt pthread_cond_wait(Pthread_cond_t*, Pthread_mutex_t*);

extern fn CInt pthread_rwlock_destroy(Pthread_rwlock_t*);
extern fn CInt pthread_rwlock_init(Pthread_rwlock_t*, Pthread_rwlockattr_t*);
extern fn CInt pthread_rwlock_rdlock(Pthread_rwlock_t*);
extern fn CInt pthread_rwlock_tryrdlock(Pthread_rwlock_t*);
extern fn CInt pthread_rwlock_trywrlock(Pthread_rwlock_t*);
extern fn CInt pthread_rwlock_unlock(Pthread_rwlock_t*);
extern fn CInt pthread_rwlock_wrlock(Pthread_rwlock_t*);

extern fn CInt pthread_rwlockattr_destroy(Pthread_rwlockattr_t*);
extern fn CInt pthread_rwlockattr_getpshared(Pthread_rwlockattr_t*, CInt*);
extern fn CInt pthread_rwlockattr_init(Pthread_rwlockattr_t*);
extern fn CInt pthread_rwlockattr_setpshared(Pthread_rwlockattr_t*, CInt);

extern fn CInt pthread_key_create(Pthread_key_t*, PosixThreadFn routine);
extern fn CInt pthread_key_delete(Pthread_key_t);
extern fn void* pthread_getspecific(Pthread_key_t);
extern fn CInt pthread_setspecific(Pthread_key_t, void* value_ptr);

extern fn CInt pthread_atfork(OnceFn prepare, OnceFn parent, OnceFn child);
extern fn void pthread_cleanup_pop(CInt execute);
extern fn void pthread_cleanup_push(PosixThreadFn routine, void* routine_arg);

extern fn int sched_yield();

module std::thread::os @if(env::POSIX && !env::LINUX);
typedef Pthread_attr_t = ulong[8];
typedef Pthread_cond_t = ulong[6];
typedef Pthread_condattr_t = ulong[8];
typedef Pthread_key_t = ulong;
typedef Pthread_mutex_t = ulong[8];
typedef Pthread_mutexattr_t = ulong[2];
typedef Pthread_once_t = ulong[2];
typedef Pthread_rwlock_t = ulong[25];
typedef Pthread_rwlockattr_t = ulong[3];
typedef Pthread_sched_param = ulong;

module std::thread::os @if(env::LINUX);
typedef Pthread_attr_t = ulong[7]; // 24 on 32bit
typedef Pthread_cond_t = ulong[6];
typedef Pthread_condattr_t = uint;
typedef Pthread_key_t = uint;
typedef Pthread_mutex_t = ulong[5]; // 24 on 32 bit
typedef Pthread_mutexattr_t = uint;
typedef Pthread_once_t = int;
typedef Pthread_rwlock_t = ulong[7]; // 32 on 3bit
typedef Pthread_rwlockattr_t = uint;
typedef Pthread_sched_param = uint;
