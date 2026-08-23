fn void a_new()
{
}

fn void a_old()
{
}

const FOO = 1;

alias a = $feat(USE_NEW) ? FOO : a_old; // #error: An alias starting with a lowercase letter

fn int main()
{
	return 0;
}
