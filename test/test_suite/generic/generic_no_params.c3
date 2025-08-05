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
	int x = values::test{}(); // #error: At least one generic parameter
}

fn void test2()
{
	List{Values{ }} v1s; // #error: At least one generic parameter
}

fn void main()
{
	List{Values{int}} v2s;
}