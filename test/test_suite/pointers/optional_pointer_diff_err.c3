module bug;
import std;

fn int main()
{
	char[1024] buf;
	char[] h = buf[128..];
	char[]? head = buf[512..];

	&head - &h; // #error: An optional value was discarded, you can assign it to a variable
	return 0;
}
