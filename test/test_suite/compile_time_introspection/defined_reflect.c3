import std;

fn int main()
{
	int x;
	$assert(!$defined($reflect("hello").name));
	$assert($defined($reflect(x).name));
	return 0;
}