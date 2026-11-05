fn int main()
{
    String msg = "Hello, C3 World!\n";
    @syscall(4, 1, (uptr)msg.ptr, msg.len); // __NR_write, STDOUT
    return 0;
}

fn void _start() @export("_start")
{
    int ret = main();
    @syscall(1, ret); // __NR_exit
}

module std::core::builtin;

alias PanicFn = fn void(String summary, String file, String function, int line, String fmt, any[] args);

PanicFn panic = &default_panic;

fn void default_panic(String summary, String file, String function, int line, String fmt, any[] args)
{
}
