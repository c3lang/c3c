module values {Type};

struct Values
{
	Type[] values;
	int count;
}

fn void test()
{

}
module main;

import values;
import std::collections::list;

fn void test1()
{
	int x = values::test{}(); // #error: requires generic arguments inside
}

fn void test2()
{
	List{Values{ }} v1s; // #error: 'Values' requires generic arguments inside the '{}', did you forget them
}

fn void main()
{
	List{Values{int}} v2s;
}