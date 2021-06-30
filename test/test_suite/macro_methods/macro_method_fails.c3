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


func void test1()
{
    An1 an;
    an.x.y.@helloWorld; // #error:  macro name must be followed by '('
}

func void test2()
{
    An2 a;
    a.@helloWorld; // #error: A macro name must be followed by '('
}

func void test3()
{
    An2 a;
    a.@helloWorld.b; // #error: There is no member or method 'b' on 'void'
}

func void test4()
{
    An1 an;
    an.x.@y; // #error: '@' should only be placed in front of macro names
}

func void test5()
{
    An1 an;
    an.x.@y(); // #error: '@' should only be placed in front of macro names
}