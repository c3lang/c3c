module test;
import std::time;

public macro timeit(#call)
{
    Time t = time::current();
    typeof(#call) result = #call;
    TimeDiff diff = time::current() - t;
    libc::printf("'%s' took %f ms\n", $stringify(#call), diff * 1000);
    return result;
}

