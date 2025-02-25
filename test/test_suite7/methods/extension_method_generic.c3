module compiler_c3;
import std::collections::list;

def IntArray = List(<int>);

extern fn void printf(char*, ...);

fn void IntArray.someFunc(IntArray *this, usz param)
{
	//do something
    this.push((int)param);
}

fn int main(String[] argv)
{
    IntArray stk;
    stk.someFunc(256);

    for (int i = 0; i < 256; i++)
    {
        stk.push(i);
    }

    for (;!stk.is_empty();)
    {
        int value = stk.pop()!!;
        printf("%i\n", value);
    }

    return 0;
}