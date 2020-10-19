macro int cofefe(#a)
{
    int y = 0;
    return #a + #a;
}

int abc = 1;

public func int xx()
{
    abc++;
    return abc;
}

public func void main()
{
    define $x = 0;
    int x = 0;
    @cofefe(x += 1);
    @cofefe(xx());
    @cofefe($x += 1); // #error: Cannot modify '$x' inside of a runtime scope.
    @cofefe(y += 1); // #error: Identifier 'y' could not be found.
}

