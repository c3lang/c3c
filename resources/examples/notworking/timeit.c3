module test;
import std::time;
import std::io;

public macro timeit(#call)
{
    Time t = time::current();
    typeof(#call) result = #call;
    TimeDiff diff = time::current() - t;
    io::printf("'%s' took %f ms\n", $stringify(#call), diff * 1000);
    return result;
}

