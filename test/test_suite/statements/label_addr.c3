import std;
fn int main(String[] args)
{
	switch FOO: (1)
	{
		case 1:
			io::printfn("%p", &FOO); // #error: It is not possible to take the address of a label
	}
	return 0;
}