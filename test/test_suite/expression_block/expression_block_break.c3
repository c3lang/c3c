module fe;
import std::io;
extern func int printf(char *str, ...);

func int main()
{
    while(true)
    {
        return {| break; return 0; |}; // #error: There is no valid target for 'break', did you make a mistake
    }
    io::println("I didn't return.");
    return 0;
}