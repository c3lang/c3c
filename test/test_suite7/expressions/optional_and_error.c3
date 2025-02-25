module testing;
import std::io;

fn void main()
{
    bool ok;
    if (ok && !foo()) io::printfn("nok"); // #error: The expression may not be an optional
}

fn bool! foo()
{
    return false;
}