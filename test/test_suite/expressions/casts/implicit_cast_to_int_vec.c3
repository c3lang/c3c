fn int main()
{
    ulong x1 = (int[<2>]){ 10, 20 } + ulong.max; // #error: is out of range for
}