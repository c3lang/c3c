// #target: linux-riscv64
module test;
import std::thread::os;

// glibc riscv64 sizes, see sysdeps/riscv/nptl/bits/pthreadtypes-arch.h
$assert(std::thread::os::Pthread_mutex_t::size == 40) : "Pthread_mutex_t size";
$assert(std::thread::os::Pthread_mutex_t::alignment == 8) : "Pthread_mutex_t alignment";
$assert(std::thread::os::Pthread_attr_t::size == 56) : "Pthread_attr_t size";
$assert(std::thread::os::Pthread_attr_t::alignment == 8) : "Pthread_attr_t alignment";
$assert(std::thread::os::Pthread_mutexattr_t::size == 4) : "Pthread_mutexattr_t size";
$assert(std::thread::os::Pthread_mutexattr_t::alignment == 4) : "Pthread_mutexattr_t alignment";
$assert(std::thread::os::Pthread_condattr_t::size == 4) : "Pthread_condattr_t size";
$assert(std::thread::os::Pthread_condattr_t::alignment == 4) : "Pthread_condattr_t alignment";
$assert(std::thread::os::Pthread_cond_t::size == 48) : "Pthread_cond_t size";
$assert(std::thread::os::Pthread_cond_t::alignment == 8) : "Pthread_cond_t alignment";
$assert(std::thread::os::Pthread_rwlock_t::size == 56) : "Pthread_rwlock_t size";
$assert(std::thread::os::Pthread_rwlock_t::alignment == 8) : "Pthread_rwlock_t alignment";
$assert(std::thread::os::Pthread_rwlockattr_t::size == 8) : "Pthread_rwlockattr_t size";
$assert(std::thread::os::Pthread_rwlockattr_t::alignment == 8) : "Pthread_rwlockattr_t alignment";
$assert(std::thread::os::Pthread_key_t::size == 4) : "Pthread_key_t size";
$assert(std::thread::os::Pthread_once_t::size == 4) : "Pthread_once_t size";

fn void main() {}
