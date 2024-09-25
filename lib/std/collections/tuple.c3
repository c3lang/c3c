module std::collections::tuple(<Type1, Type2>);

struct Tuple @adhoc
{
	Type1 first;
	Type2 second;
}

module std::collections::triple(<Type1, Type2, Type3>);

struct Triple @adhoc
{
	Type1 first;
	Type2 second;
	Type3 third;
}