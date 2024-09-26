module test;
import std;

fn void main()
{
    char[*] z = { 1, 2 };
    char[] y = z; // #error: Conversions from arrays or vectors
}
