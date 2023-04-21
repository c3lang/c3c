module hello_world;
import std;
import bar;
$if (env::os_is_win32())
fn int test_doubler(int x)
{
	return x * x;
}
$else
extern fn int test_doubler(int);
$endif
extern fn void printf(char *, ...);

fn int main()
{
    printf("Hello World!\n");
    bar::test();
    printf("Hello double: %d\n", test_doubler(11));
    return 17;
}