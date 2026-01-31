union Xu
{
    int[int.max] b; // #error: Union member 'b' would cause the union to become too large
}