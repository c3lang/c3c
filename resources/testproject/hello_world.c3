module hello_world;
import bar;

extern fn void printf(char *hello);

fn int main(int x)
{
    printf("Hello World!\n");
    bar::test();
    return 1;
}