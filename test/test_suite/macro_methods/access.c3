struct An1
{
    An3 x;
}

struct An3
{
    An2 y;
}

struct An2
{
}

extern func void printf(char *string);

macro void An2.helloWorld(An2 *an2)
{
    printf("An2 hello\n");
}

func void check()
{
    printf("Checking\n");
}


func void main()
{
    An1 an;
    an.x.y.@helloWorld();
    An2 a;
    a.@helloWorld();
}