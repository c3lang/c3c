import std::io;

fn void main()
{
    io::printfln("min(-1, 1) == %s", min(-1, 1));
    io::printfln("max(-1, 1) == %s", max(-1, 1));
    io::printfln("min(-1, 0, 1) == %s", min(-1, 0, 1));
    io::printfln("max(-1, 0, 1) == %s", max(-1, 0, 1));
}
