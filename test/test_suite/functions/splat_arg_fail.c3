macro test1(...) {}
macro test(...) => test1(...args); // #error: 'args' could not be found

fn int main(String[] args)
{
	test();
	return 0;
}
