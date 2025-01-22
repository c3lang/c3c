module hello_world;
import std;
import bar;
import clib;
import clib2;

fn int test_doubler(int x) @if(env::WIN32) => x * x;
extern fn int test_doubler(int) @if(!env::WIN32);
extern fn void printf(char *, ...);

fn int main()
{
    printf("Hello World!\n");
    bar::test();
    printf("Hello double: %d\n", test_doubler(11));
	if ($feature(ABCD)) io::printn("ABCD");
	clib::hello_from_c();
	clib2::hello_from_c_zip();
    return 0;
}