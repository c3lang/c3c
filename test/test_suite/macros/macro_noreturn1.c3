import std;
macro @gogo_abort() @noreturn => os::exit(1);

fn void main()
{
    File? f = file::open("/tmp/ooo", "w");
    if (catch err = f) @gogo_abort();
    File g = f;
}