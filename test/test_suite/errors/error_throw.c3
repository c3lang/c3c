module foo;

fault Blurg
{
    Z
}

fn void main()
{
    int! i = Blurg.Z!;
    int! j = Blurg.Z!;
}