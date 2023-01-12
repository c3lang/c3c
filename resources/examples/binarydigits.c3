module binarydigits;
import std::math;
import std::io;
fn void main()
{
    for (int i = 0; i < 20; i++)
    {
        VarString s = bin(i);
        defer s.destroy();
        io::printf("%s\n", s);
    }
}

fn VarString bin(int x)
{
    int bits = x == 0 ? 1 : 1 + (int)math::log2(x);
    VarString str;
    str.append_repeat('0', bits);
    for (int i = 0; i < bits; i++)
    {
        str.set((usz)(bits - i - 1), x & 1 ? '1' : '0');
        x >>= 1;
    }
    return str;
}