extern fn void printf(...);

struct An1
{
    An3 x;
}

struct An3
{
    An2 y;
}

alias AnCall = fn void();

struct An2
{
    AnCall t;
}

fn void An2.helloWorld(An2 *an2)
{
    printf("An2 hello\n");
}

fn void check()
{
    printf("Checking\n");
}


fn void main()
{
    An1 an;
    an.x.y.helloWorld();
    an.x.y.t = &check;
    an.x.y.t();
}