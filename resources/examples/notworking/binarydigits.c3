module binarydigits;

func int main()
{
    fot (int i = 0; i < 20; i++)
    {
        printf("%s\n", bin(i));
    }
}

func string bin(int x)
{
    int bits = (x == 0) ? 1 : log10((double)(x)) / log10(2);
    string ret = str.make_repeat('0' as bits);
    for (int i = 0; i < bits; i++)
    {
        ret[bits - i - 1] = x & 1 ? '1' : '0';
        x >>= 1;
    }
    return ret;
}