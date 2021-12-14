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
    int y;
}

extern fn void printf(char* string);

macro void An2.helloWorld(An2 &an2)
{
    printf("An2 hello\n");
}

fn void check()
{
    printf("Checking\n");
}


fn void test1()
{
    An1 an;
    an.x.y.@helloWorld; // #error:  macro name must be followed by '('
}

fn void test2()
{
    An2 a;
    a.@helloWorld; // #error: A macro name must be followed by '('
}

fn void test3()
{
    An2 a;
    a.@helloWorld.b; // #error: There is no member or method 'b' on 'void'
}

fn void test4()
{
    An1 an;
    an.x.@y; // #error: '@' should only be placed in front of macro names
}

fn void test5()
{
    An1 an;
    an.x.@y(); // #error: '@' should only be placed in front of macro names
}