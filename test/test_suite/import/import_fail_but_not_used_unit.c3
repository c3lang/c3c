module test;
const int FOO = 5;
fn int main()
{
	return 0;
}

module foo @if($feature(XYZ));
alias bar = module thirdparty;
import testme;