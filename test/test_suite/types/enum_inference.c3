
enum Inf
{
    A,
    B,
    C
}

enum Inf2 : char
{
    A,
    B,
    C,
}

alias BooInf = Inf;


fn void enumInferenceTest()
{
    Inf x = Inf.A;
    x = BooInf.B;
    x = A;
    int x1 = 0;
    bool y = x1 == x1;
    Inf2 z = C;
    if (z == Inf2.A) return;
    if (z == (Inf2)1) return;
    z = (Inf2)2;
    switch (z)
    {
        case Inf2.A:
            x1++;
            return;
        case B:
            return;
        case (Inf2)2:
            x1 += 1;
            return;
        default:
            return;
    }
}