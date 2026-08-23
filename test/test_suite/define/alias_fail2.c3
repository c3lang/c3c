fn void a_new()
{
}

fn void a_old()
{
}

const FOO = 1;

alias a = $feat(USE_NEW) ?: a_old; // #error: A function name cannot be used as a value, did you want to pass it by pointer?

fn int main()
{
	return 0;
}