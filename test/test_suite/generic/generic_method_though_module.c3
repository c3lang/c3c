module abc;
import hello;

fn void test() @if(!$defined(Maybe2{int}.hello)) // #error: Instantiation of the generic type 'Maybe2' happens before methods are registered, please try to rearrange the instantiation order.
{}

fn void main() {
  test();
}

module hello <Type>;

struct Maybe2
{
	Type value;
	bool has_value;
}

fn bool Maybe2.equals(self, Maybe2 other) @operator(==) @if($defined(Type.equals) || true)
{
	return false;
}