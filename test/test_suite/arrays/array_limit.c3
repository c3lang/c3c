fn int main()
{
    int[0x80000000][1] x;  // #error: This type would exceed max type size of 2 GB.
    return 0;
}