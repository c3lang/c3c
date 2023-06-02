import std::io;
def Tester = fn int(int x, int y, int z);

fn int test1(
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 20
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 100
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // #error:  The number of params exceeded the max of 127. To accept more arguments, consider using varargs
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int // 200
) { return 1; }

fn int test2(
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 20
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 100
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // #error: The number of params exceeded the max of 127.
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 200
	int... x
) { return 1; }

