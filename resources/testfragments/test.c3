import std::io;

extern func int printf(char* message, ...);

macro void swap(&a, &b)
{
    typeof(a) temp = a;
    a = b;
    b = temp;
}

func void main()
{
    int x = 1;
    int y = 2;
    @swap(x, y);
    printf("x: %d y: &d\n", x, y);
}