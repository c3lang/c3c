struct Abc
{
	int[256] x;
}
union Xu
{
    Abc[0x40000000] b; // #error: Union member 'b' would cause the union to become too large
}