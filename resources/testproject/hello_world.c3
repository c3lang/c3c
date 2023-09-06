module hello_world;
import std;
import bar;

fn int test_doubler(int x) @if(env::WIN32) => x * x;
extern fn int test_doubler(int) @if(!env::WIN32);
extern fn void printf(char *, ...);

fn int main()
{
    printf("Hello World!\n");
    bar::test();
    printf("Hello double: %d\n", test_doubler(11));
    return 0;
}