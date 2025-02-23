import std;

fn void loop_start() => io::printn("Loop Start");
fn void loop_end() => io::printn("Loop End");
fn void do_something() => io::printn("Do something");

macro @thing(; @body())
{
  loop_start();
  @body();
  loop_end();
}

fn void strcpy(char *s1, char *s2) {
  while (*s1++ = *s2++);
}

fn void main()
{
	while (true) @thing()
	{
        do_something();
        break;
	};
	for (;;) @thing()
    {
        do_something();
		break;
	};
	if (true) @thing()
	{
		do_something();
	};
}