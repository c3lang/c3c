module hello_world;
import bar;

extern fn int test_doubler(int);
extern fn void printf(char *, ...);

fn int main(int x)
{
    printf("Hello World!\n");
    bar::test();
    printf("Hello double: %d\n", test_doubler(11));
    return 1;
}