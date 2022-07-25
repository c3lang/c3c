module binarydigits;
import std::math;
import std::io;
fn void main()
{
    for (int i = 0; i < 20; i++)
    {
        String s = bin(i);
        defer s.destroy();
        io::printf("%s\n", s);
    }
}

fn String bin(int x)
{
    int bits = 1 + (int)(x == 0 ? 0 : math::log10((double)(x)) / math::log10(2));
    String str;
    str.append_repeat('0', bits);
    for (int i = 0; i < bits; i++)
    {
        str.set((usize)(bits - i - 1), x & 1 ? '1' : '0');
        x >>= 1;
    }
    return str;
}