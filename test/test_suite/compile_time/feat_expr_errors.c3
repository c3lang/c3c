module test;

fn void test_wrong_op()
{
	$assert $feat(MACOS && WIN32); // #error: One or more feature names were expected here
}

fn void test_path()
{
	$assert $feat(std::MACOS); // #error: A feature name cannot have a path
}

fn void test_not_const()
{
	int foo = 1;
	$assert $feat(foo); // #error: A feature name was expected here, features are all uppercase
}

fn void test_literal()
{
	$assert $feat(123); // #error: One or more feature names were expected here
}
