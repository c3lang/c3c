module std::os::darwin @if(env::DARWIN);

extern fn usz malloc_size(void* ptr);
