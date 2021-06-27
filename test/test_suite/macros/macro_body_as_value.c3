macro foo4(;@body)
{
    @body; // #error: must be followed by ()
}

func void test()
{
    @foo4()
    {
      int x = 0;
    };
}