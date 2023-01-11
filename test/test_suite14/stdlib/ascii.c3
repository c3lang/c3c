import std::io;
import std::ascii;

fn void main()
{
    for(char c = 0; c < 255; c++)
    {
        io::printfn("%c (%s) is...:", c, c);
        io::printfn("lower: %s; upper: %s; digit: %s; bdigit: %s; odigit: %s; xdigit: %s; alpha: %s; print: %s; graph: %s; space: %s; alnum: %s; punct: %s; blank: %s; cntrl: %s;",
                     c.is_lower(), c.is_upper(), c.is_digit(), c.is_bdigit(), c.is_odigit(), c.is_xdigit(), c.is_alpha(), c.is_print(),
                     c.is_graph(), c.is_space(), c.is_alnum(), c.is_punct(), c.is_blank(), c.is_cntrl());
    }

    foreach(c : "0123456789abcdefghijklmnopqrstuvwxyz")
    {
        io::printfn("'%c'.to_upper(): %c", c, c.to_upper());
    }
    foreach(c : "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    {
        io::printfn("'%c'.to_lower(): %c", c, c.to_lower());
    }
}




