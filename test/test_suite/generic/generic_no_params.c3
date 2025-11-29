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
	int x = values::test{}(); // #error: must be instantiatied with generic
}

fn void test2()
{
	List{Values{ }} v1s; // #error: must be instantiatied with generic
}

fn void main()
{
	List{Values{int}} v2s;
}