module baz;
import bar;

struct Foo
{
    int x;
    int y;
}



//define bar::blub(Foo, 1) as fooblub;

public func void main()
{
    Foo f = { 3, 4 };
    //Foo g = fooblub(&f);
}