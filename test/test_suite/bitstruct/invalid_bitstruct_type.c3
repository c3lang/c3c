
bitstruct Test : float // #error: The type of the bitstruct cannot be 'float' but must be an integer
{
    int a : 1..3;
    int b : 5..10;
    uint c : 20..20;
}