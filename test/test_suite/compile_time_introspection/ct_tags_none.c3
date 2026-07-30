module std::os::posix;

fn void foo() <TEST> @if($reflect(env::POSIX).tags) // #error: Expected a boolean value not 'String[]'
{}

fn int main()
{
	foo{1}();
	return 0;
}