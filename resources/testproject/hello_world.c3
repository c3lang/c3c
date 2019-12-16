module hello_world;
import bar;

extern func void printf(char *hello);

func int main(int x)
{
    printf("Hello World!\n");
    bar::test();
    return 1;
}