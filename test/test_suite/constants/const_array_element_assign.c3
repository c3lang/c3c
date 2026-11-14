module test;
struct Foo { int x; int[2] arr; }
const char[4] CA = "AAA";
const int[2][2] CA2 = { { 1, 2 }, { 3, 4 } };
const Foo F = { 1, { 2, 3 } };
const Foo[2] FA = { { 1, { 2, 3 } }, { 1, { 2, 3 } } };
const int[<2>] CV = { 1, 2 };
const int* P = null;

fn void test()
{
	const int[2] LA = { 1, 2 };
	int i = 1;
	CA[0] = 66; // #error: You cannot assign to an element of a constant
	LA[0] = 5; // #error: You cannot assign to an element of a constant
	CA2[0][1] = 5; // #error: You cannot assign to an element of a constant
	F.arr[0] = 5; // #error: You cannot assign to an element of a constant
	FA[0].x = 5; // #error: You cannot assign to an element of a constant
	CV[0] = 5; // #error: You cannot assign to an element of a constant
	CA[0] += 1; // #error: You cannot assign to an element of a constant
	CA[0]++; // #error: You cannot assign to an element of a constant
	CA[i] = 66; // #error: You cannot assign to an element of a constant
	P[0] = 5;
}
