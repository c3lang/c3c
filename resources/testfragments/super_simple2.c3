module baz;
import gen;

extern func int printf(char *hello, ...);
extern func void blurg();

const $FOO = 100;



struct Foo
{
    int x;
}

struct Bar
{
    Foo f;
    int y;
}

func void Foo.add(Foo *foo)
{
    foo.x++;
}

public func void runBaz()
{
    Foo foo = { 7 };
    printf("%d\n", foo.x);
    foo.add();
    printf("%d\n", foo.x);
    foo.add();
    printf("%d\n", foo.x);
    Bar bar = {};
       $switch (typeof(bar))
        {
            $case int:
                printf("Was int\n");
            $case Foo:
                printf("Was Foo\n");
            $case Bar:
                printf("Was Bar\n");
            $default:
                printf("Was not int\n");
        }
               $switch ($FOO)
                {
                    $case 0:
                        printf("Was 0\n");
                    $case 1:
                        printf("Was 1\n");
                    $default:
                        printf("Was not either\n");
                }

}


func void notVisible()
{

}