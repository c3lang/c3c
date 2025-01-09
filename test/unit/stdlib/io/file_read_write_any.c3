module std::io::file @test;

import std::io;

struct Data {
  bool boolean;
  char byte;
  ichar s_byte;
  ushort word;
  short s_word;
  uint dword;
  int s_dword;
  ulong qword;
  long s_qword;
  uint128 dqword;
  int128 s_dqword;
  char[128] data;
  bool[57] flags;
  bitstruct : ushort {
    char upper : 8..15;
    char lower : 0..7;
  }
  bitstruct : ushort @overlap {
    ushort total : 0..15;
    char t_lower : 0..7;
    char t_upper : 8..15;
    bool t_rand : 12;
    bool t_other : 4;
  }
  union {
    char hello;
    short hallo;
    uint byebye;
  }
}

fn void read_write_any()
{
    Data data;
    data.boolean = false;
    data.byte = 0xAF;
    data.s_byte = -56;
    data.word = 0xBEAF;
    data.s_word = -547;
    data.qword = 0xCAFEAFBC;
    data.s_qword = -2000000000;
    data.dqword = 0xCAFEADDEAFBEFF;
    data.s_dqword = -45600000000000000;
    for (int i = 0; i < 128; ++i)
      data.data[i] = (char)(255 - i);
    for (int i = 0; i < 57; ++i)
      data.flags[i] = (i % 4) == 0;
    data.upper = 0x56;
    data.lower = 0x44;
    data.total = 0xA55A;
    data.hello = 0xCC;

    File file = file::open("__file_read_write_any_test_file", "wb")!!;

    io::write_any(&file, &data)!!;
    file.flush()!!;
    file.close()!!;

    file = file::open("__file_read_write_any_test_file", "rb")!!;
    Data rdata;
    io::read_any(&file, &rdata)!!;

    file.close()!!;

    assert(rdata.boolean == data.boolean);
    assert(rdata.byte == data.byte);
    assert(rdata.s_byte == data.s_byte);
    assert(rdata.word == data.word);
    assert(rdata.s_word == data.s_word);
    assert(rdata.dword == data.dword);
    assert(rdata.s_dword == data.s_dword);
    assert(rdata.qword == data.qword);
    assert(rdata.s_qword == data.s_qword);
    assert(rdata.dqword == data.dqword);
    assert(rdata.data == data.data);
    assert(rdata.flags == data.flags);
    assert(rdata.upper == data.upper);
    assert(rdata.lower == data.lower);
    assert(rdata.t_lower == data.t_lower);
    assert(rdata.t_upper == data.t_upper);
    assert(rdata.t_rand == data.t_rand);
    assert(rdata.t_other == data.t_other);
    assert(rdata.total == data.total);
    assert(rdata.hello == data.hello);
    assert(rdata.hallo == data.hallo);
    assert(rdata.byebye == data.byebye);
}