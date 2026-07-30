module std::os::posix @feat(POSIX);

extern fn CInt posix_memalign(void **memptr, usz alignment, usz size);