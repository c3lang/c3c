fn int main()
{
    int[1024 * 1024 * 1024][6] x;  // #error: This type would exceed max type size of 1 GB.
    return 0;
}