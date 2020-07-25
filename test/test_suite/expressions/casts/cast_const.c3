func void test1()
{
    char  a = cast(256 + 1 as char);
    ushort b = cast(65536+1 as ushort);
    char c = cast(65536+400 as ushort);  // #error: Cannot implicitly cast 'ushort' to 'char'
}
