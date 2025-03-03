module test;

def abc = ggg;
tlocal int ggg = 123;

int[2] x = { abc, 1 }; // #error: expression must be a constant

fn int main()
{
	return 0;
}