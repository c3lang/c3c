import std::io;

struct MyStruct
{
  DString* dyn;
}

fn void main()
{
  @pool()
  {
    usz values_len = 10;

    MyStruct ms = {
      .dyn = mem::temp_new_array(DString, values_len).ptr,
    };

    for (usz i; i < values_len; ++i)
    {
      ms.dyn[i].temp_init();
    }

    ms.dyn[0].append_chars("sad");
    ms.dyn[0].append("sad");
  };
}