module test;
import libc;

faultdef NOPE;

fn int? eventually_succeed()
{
	static int i = 0;
	if (i++ < 3) return NOPE~;
	return i * 3;
}

macro @retry(#function, int retries = 3)
{
	var $Type = $typeof(#function);
    fault e;
    do
    {
        $Type? result = #function;
        if (catch err = result)
        {
            e = err;
            continue;
        }
        return result;
    } while (retries-- > 0);
    return e~;
}

fn void main()
{
    int? result = @retry(eventually_succeed());
    if (try result)
    {
	    libc::printf("Got result: %d\n", result);
    }
    else
    {
        libc::printf("Failed :(\n");
    }
}