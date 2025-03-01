
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

def BooInf = Inf;


fn void enumInferenceTest()
{
    Inf x = Inf.A;
    x = BooInf.B;
    x = A;
    int x1 = 0;
    bool y = x1 == x1;
    Inf2 z = C;
    if (z == Inf2.A) return;
    if (z == Inf2.from_ordinal(1)) return;
    z = Inf2.from_ordinal(2);
    switch (z)
    {
        case Inf2.A:
            x1++;
            return;
        case B:
            return;
        case Inf2.from_ordinal(2):
            x1 += 1;
            return;
        default:
            return;
    }
}