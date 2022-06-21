bitstruct test : int // #error: Names of bitstructs must start with an upper case letter.
{
    int a : 1..3;
    int b : 5..10;
    uint c : 20..20;
}

bitstruct $if : int // #error: 'bitstruct' should be followed by the name of the bitstruct.
{
    int a : 1..3;
    int b : 5..10;
    uint c : 20..20;
}

bitstruct if : int // #error: Names of bitstructs must start with an upper case letter.
{
    int a : 1..3;
    int b : 5..10;
    uint c : 20..20;
}