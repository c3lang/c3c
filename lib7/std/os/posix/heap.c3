module std::os::posix @if(env::POSIX);

extern fn CInt posix_memalign(void **memptr, usz alignment, usz size);