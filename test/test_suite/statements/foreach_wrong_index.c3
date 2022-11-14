import std::io;

struct Foo
{
    int[3] elements;
}

fn int Foo.at(Foo *vector, float index) @operator([])
{
    return 1;
}

fn int Foo.len(Foo *vector) @operator(len)
{
    return 3;
}

fn void main()
{

    Foo f;
    io::printfln("%s", f[12.2]);
    foreach (int i, value : f) // #error: Only integer
    {
        io::printfln("v[%s] = %s", i, value);
    }
}