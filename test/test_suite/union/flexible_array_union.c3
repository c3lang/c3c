union Zee
{
    int z;
    int[*] y; // #error: Flexible array members not allowed in unions.
}