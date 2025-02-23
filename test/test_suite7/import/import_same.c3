module foo::abc;
import bar;

fn int test1()
{
	return abc::test();
}

module bar::abc;
fn int test() { return 1; }