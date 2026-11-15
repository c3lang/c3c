module hello_world;

fn void print(String msg)
{
    @syscall(1, 1, (uptr)msg.ptr, (uptr)msg.len); // __NR_write, STDOUT
}

fn void printn(String msg)
{
    print(msg);
    print("\n");
}

fn void exit(int exit_code)
{
    @syscall(60, (uptr)exit_code); // __NR_exit
}

fn int main()
{
    printn("Hello, C3 World!");
    return 0;
}

fn void _start() @export("_start")
{
    exit(main());
}

module std::core::builtin;

<*
 @require $defined(uptr[] x = { ...$vaarg }) : "Arguments must be convertible to uptr"
*>
macro uptr @syscall(...) @builtin
{
	return $$syscall(...$vaarg);
}
