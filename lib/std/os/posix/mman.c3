module std::os::posix @if(env::POSIX);
import libc;


const PROT_NONE     = 0x00;    // no permissions
const PROT_READ     = 0x01;    // pages can be read
const PROT_WRITE    = 0x02;    // pages can be written
const PROT_EXEC     = 0x04;    // pages can be executed

const MAP_SHARED    = 0x0001;  // share changes
const MAP_PRIVATE   = 0x0002;  // changes are private

const MAP_FILE       = 0x0000; // map from file (default)

const MAP_ANONYMOUS  =  env::LINUX ??? 0x20 : 0x1000; // allocated from memory, swap space

const void* MAP_FAILED = (void *)(uptr)-1; // mmap failed
const MADV_NORMAL     = 0; // no further special treatment
const MADV_RANDOM     = 1; // expect random page refs
const MADV_SEQUENTIAL = 2; // expect sequential page refs
const MADV_WILLNEED   = 3; // will need these pages
const MADV_DONTNEED   = 4; // dont need these pages

extern fn void* mmap(void*, usz, CInt, CInt, CInt, Off_t);
extern fn CInt munmap(void*, usz);
extern fn CInt mprotect(void*, usz, CInt);
extern fn int madvise(void*, usz, CInt);

extern fn CInt getpagesize();