// #target: macos-x64

module foo;


bitstruct BitField
{// #error: followed by bitstruct type
  int a : 0..2;
  int b : 4..6;
  int c : 7..8;
}


bitstruct BitField2 : char
{
  int a : 0..2;
  int b : 4..6;
  int c : 7..8;
}

struct Packet
{
  bitstruct : int
  {
    int a : 0..2;
    int b : 3..5;
    int c : 5..6;
  }
  int packet_id;
}

bitstruct BitField3 : char[3]
{
  int a : 0..2;
  int b : 3..8;
  int c : 9..18;
  int d : 19..23;
}

bitstruct BitField4 : char[3] @align(8)
{
  int a : 0..2;
  int b : 3..7;
  int c : 8..15;
  int d : 16..19;
}
