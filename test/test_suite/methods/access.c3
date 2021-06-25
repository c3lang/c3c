extern func void printf(...);

struct An1
{
    An3 x;
}

struct An3
{
    An2 y;
}

define AnCall = func void();

struct An2
{
    AnCall t;
}

func void An2.helloWorld(An2 *an2)
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
    an.x.y.helloWorld();
    an.x.y.t = &check;
    an.x.y.t();
}