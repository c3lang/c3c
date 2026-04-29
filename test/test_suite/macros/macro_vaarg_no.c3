import std;

macro void example(String... x)
{
  $for var $i = 0; $i < $vaarg.len; $i++: // #error: can only be used inside of a macro with untyped
    io::printn($vaarg[$i]);
  $endfor
}

fn void main()
{
	example();
}

