import std::io;

fn void main()
{
    io::printfn("min(-1, 1) == %s", min(-1, 1));
    io::printfn("max(-1, 1) == %s", max(-1, 1));
    io::printfn("min(-1, 0, 1) == %s", min(-1, 0, 1));
    io::printfn("max(-1, 0, 1) == %s", max(-1, 0, 1));
}
