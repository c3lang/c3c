module fe;
import std::io;
extern fn int printf(char *str, ...);

fn int main()
{
    while(true)
    {
        return {| break; return 0; |}; // #error: There is no valid target for 'break', did you make a mistake
    }
}