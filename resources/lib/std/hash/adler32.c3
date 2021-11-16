module std::hash::adler32;

private const uint ADLER_CONST = 65521;

struct Adler32
{
    uint a;
    uint b;
}

fn void Adler32.init(Adler32 *this)
{
    *this = { 1, 0 };
}

fn void Adler32.updatec(Adler32* this, char c)
{
    this.a = (this.a + c) % ADLER_CONST;
    this.b = (this.b + this.a) % ADLER_CONST;
}

fn void Adler32.update(Adler32* this, char[] data)
{
    uint a = this.a;
    uint b = this.b;
    foreach (char x : data)
    {
        a = (a + x) % ADLER_CONST;
        b = (b + a) % ADLER_CONST;
    }
    *this = { a, b };
}

fn uint Adler32.final(Adler32* this)
{
    return (this.b << 16) | this.a;
}

fn uint encode(char[] data)
{
    uint a = 1;
    uint b = 0;
    foreach (char x : data)
    {
        a = (a + x) % ADLER_CONST;
        b = (b + a) % ADLER_CONST;
    }
    return (b << 16) | a;
}