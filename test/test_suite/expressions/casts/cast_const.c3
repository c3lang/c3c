func void test1()
{
    ichar  a = cast(256 + 1 as ichar);
    ushort b = cast(65536+1 as ushort);
    ichar c = cast(65536+400 as ushort);  // #error: Cannot implicitly cast 'ushort' to 'ichar'
}
