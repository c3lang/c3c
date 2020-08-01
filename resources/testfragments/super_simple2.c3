module baz;
import gen;

extern func int printf(char *hello, ...);
extern func void blurg();

public func void runBaz()
{

    int i = printf("hello");

    if (i > 0) blurg();

    if (i > 0) return;

    $unreachable;


}


func void notVisible()
{

}