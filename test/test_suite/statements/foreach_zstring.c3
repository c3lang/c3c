module get_environ;
import std;

fn void main(String[] args)
{
    ZString* environ_arr = null;
    foreach (idx, ZString &envvar : environ_arr) // #error: It's not possible to enumerate an expression of type 'ZString*'
    {}
}