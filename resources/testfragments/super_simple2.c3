module baz;
import gen;

extern func int printf(char *hello, ...);

public func void runBaz()
{
    int x = printf("Baz func\n");
    int y = printf("FOek");

    assert(x > 0 && y > 0);

    int z = printf("Foefe\n");
    x = printf("Baz func\n");
    y = printf("FOek");

    assert(x > 0 && y > 0 && printf("Fejei") > 0);


}


func void notVisible()
{

}