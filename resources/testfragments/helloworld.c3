module helloworld;

extern fn void printf(char *str, ...);

fn void main()
{
    printf("Hello World!\n");
}