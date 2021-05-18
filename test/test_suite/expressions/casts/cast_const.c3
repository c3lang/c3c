func void test1()
{
    ichar  a = (ichar)(256 + 1);
    ushort b = (ushort)(65536+1);
    ichar c = (ushort)(65536+400);  // #error: Cannot implicitly cast 'ushort' to 'ichar'
}
