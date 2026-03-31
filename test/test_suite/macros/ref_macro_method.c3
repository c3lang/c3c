import std::io;

struct MyStruct
{
  DString* dyn;
}

fn void main()
{
  @pool()
  {
    sz values_len = 10;

    MyStruct ms = {
      .dyn = mem::temp_array(DString, values_len).ptr,
    };

    for (sz i; i < values_len; ++i)
    {
      ms.dyn[i].tinit();
    }

    ms.dyn[0].append_string("sad");
    ms.dyn[0].append("sad");
  };
}