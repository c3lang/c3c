module oups;
import std::io;

fn void main()
{
    recm(1);
}

macro recm(x)
{
    recm(x + 1); // #error: Failure
    recm(x + 1);
}