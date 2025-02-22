module std::os::linux @if(env::LINUX);
extern fn usz malloc_usable_size(void* ptr);