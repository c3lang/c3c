module baz;
import bar;

struct Foo
{
    int x;
    long z;
    int y;
    struct fe
    {
        int dd;
        int ee;
    }
}
struct Bar
{
    int[1024] x;
    int y;
}

public func Bar barCopy(Bar *bar)
{
    Bar copy = *bar;
    copy.x[0] = copy.y;
    return copy;
}
//define bar::blub(Foo, 1) as fooblub;

extern func int blurbi();
extern func int oefk(int i);

public func void main()
{
    Foo f = {};
    usize x = Foo.sizeof;
    usize y = int.sizeof;
    usize z = int.alignof;
    usize w = Foo.alignof;
    typeid t = Foo.x.typeid;
    typeid aa = Foo.typeid;
    typeid bb = int.typeid;
    typeid cc = Foo.fe.typeid;
    typeid dd = Foo.fe.dd.typeid;
    char* str = Foo.nameof;
    char* str2 = int.nameof;
    char* str3 = Foo.qnameof;
    char* str4 = int.qnameof;
    char* foo = (int*).nameof;
    int blurb = void;
    Foo foek = { x = blurbi() };
    Foo floed = void;
    if (foek.x != 2)
    {
        oefk(floed.x);
    }
    else
    {
        oefk(foek.x);
    }
    //int fe = Foo.x.offsetof;
    // usize vv = Foo.x.offsetof;
  //  f = fooblud(f);
}