module test;
import std;
import some_module;

fn int main(String[] args)
{
	int z = some_module::A_CONST{22};
	return 0;
}

module some_module <F>;
import std;
const A_CONST = 12 + 2 * math::log2(F / 25); // #error: This expression cannot be evaluated at compile time