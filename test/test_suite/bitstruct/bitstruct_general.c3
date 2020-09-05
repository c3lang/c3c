// #skip

module foo;


bitstruct BitField
{
  int a : 3;
  int b : 3;
  int c : 2;
}


bitstruct BitField2 : byte
{
  int a : 3;
  int b : 3;
  int c : 2;
}

struct Packet
{
  bitstruct bitfield : int
  {
    int a : 3;
    int b : 3;
    int c : 2;
    int pad : 24;
  }
  int packet_id;
}

bitstruct BitField3 : byte[3]
{
  int a : 3;
  int b : 6;
  int c : 10;
  int d : 5;
}

bitstruct BitField3 : byte[3] @aligned
{
  int a : 3;
  int b : 5;
  int c : 8;
  int d : 5;
  void  : 5;
}
