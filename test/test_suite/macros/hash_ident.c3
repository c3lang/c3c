macro int @cofefe(#a)
{
    int y = 0;
    return #a + #a;
}

int abc @private = 1;

fn int xx()
{
    abc++;
    return abc;
}

fn void main()
{
    var $x = 0;
    int x = 0;
    @cofefe(x += 1);
    @cofefe(xx());
    $typeof($x += 1) xx;
    $assert $x == 1;
    @cofefe(y += 1); // #error: 'y' could not be found
}

