// #skip

module foo;


bitstruct BitField
{
  int a : 3;
  int b : 3;
  int c : 2;
}


bitstruct BitField2 : char
{
  int a : 3;
  int b : 3;
  int c : 2;
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
  int a : 3;
  int b : 6;
  int c : 10;
  int d : 5;
}

bitstruct BitField3 : char[3] @aligned
{
  int a : 3;
  int b : 5;
  int c : 8;
  int d : 5;
  void  : 5;
}
