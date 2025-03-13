import std::io;
alias Tester = fn int(int x, int y, int z);

fn int test1(
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 20
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 100
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 120
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 200
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 220
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 240
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // #error:  The number of params exceeded
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int // 300
) { return 1; }

fn int test2(
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 20
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 100
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 120
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 200
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 220
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 240
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // #error:  The number of params exceeded
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, //
	int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, int, // 300
	int... x
) { return 1; }

