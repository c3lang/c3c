fn int main()
{
    String msg = "Hello, C3 World!\n";
    $$syscall(4, 1, (uptr)msg.ptr, msg.len); // __NR_write, STDOUT
    return 0;
}

fn void _start() @export("_start")
{
    int ret = main();
    $$syscall(1, ret); // __NR_exit
}

module std::core::builtin;

def PanicFn = fn void(String message, String file, String function, uint line);

PanicFn panic = &default_panic;

fn void default_panic(String message, String file, String function, uint line)
{
}
