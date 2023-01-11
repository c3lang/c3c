fn void test()
{
    int[6] feok2 = { 1, 8, 100, 293, 23982, 34};
    int[] feok = &feok2;
    int[] flok = feok2[3..5];
    int[] flak = flok[1..2];
    flok = feok2[..5];
    flok = feok2[..^2];
    flok = feok2[..];
    flok = feok2[^3..];
    flok = feok2[^4..5];
    flok = feok2[2..^2];
    flok = feok2[^3..^1];
    flok = feok2[..];
    flak = flok[..6];
    flak = flok[..^2];
    flak = flok[..];
    flak = flok[^3..];
    flak = flok[^4..5];
    flak = flok[2..^2];
    flak = flok[^3..^1];
    int* p = null;
    // TODO p[-1..20];
}