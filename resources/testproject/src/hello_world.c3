module hello_world;
import std, bar, clib, clib2;

fn int test_doubler(int x) @if(env::WIN32) => x * x;
extern fn int test_doubler(int) @if(!env::WIN32);
extern fn void printf(char *, ...);

fn int main()
{
    printf("Hello World!\n");
    printf("Authors:");
    io::printn(env::AUTHORS);
    printf("Author emails:");
    io::printn(env::AUTHOR_EMAILS);

    (void)io::printfn("Home dir: %s", path::home_directory(tmem));
    (void)io::printfn("Desktop dir: %s", path::desktop_directory(tmem));
    (void)io::printfn("Documents dir: %s", path::documents_directory(tmem));

    bar::test();
    printf("Hello double: %d\n", test_doubler(11));
	if ($feature(ABCD)) io::printn("ABCD");
	clib::hello_from_c();
	clib2::hello_from_c_zip();
    return 0;
}