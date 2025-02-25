import std::io;

struct Test {
ushort a;
  bitstruct : ushort @overlap {
    ushort ab : 0..15;
    char a : 8..15; // #error: Duplicate member name 'a'
    char b : 0..7;
    bool c : 7;
    bool d : 6;
    bool e : 5;
    bool f : 4;
  }
}

fn void main() {
  io::printfn("Weird structs :P");

  Test test;
  test.ab = 0xAFBA;

  io::printfn("%02x %02x -> %04x\n", test.a, test.b, test.ab);
  io::printfn("%x %x %x %x\n", test.c, test.d, test.e, test.f);
}