import std;

fn String? test()
{
	return "";
}
macro String? test2() => test();

fn void? print_nel()
{
    while (String? line = test2()!) // #error: Expected a non-optional variable
    {
    }
}

fn int main() { (void)print_nel(); return 0; }