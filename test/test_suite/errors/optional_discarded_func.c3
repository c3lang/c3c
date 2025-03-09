
fn int abc(int x) { return 1; }
fn int? def2(int y) @maydiscard { return 1; }
fn int? def3(int z) { return 1; }

fn void test1()
{
	int? x;
	abc(x); // #error: The result of this call is optional due to its argumen
}

fn void test2()
{
	int? x;
	int y;
	abc(y);
	abc(x) + 4; // #error: An optional value may not be discarded
}

fn void test3()
{
	int? x;
	int y;
	def2(1);
	def2(x); // #error: The result of this call is optional due to its argumen
}

fn void test4()
{
	int? x;
	int y;
	def2(1);
	def2(x) + 4; // #error: An optional value may not be discarded
}

fn void test5()
{
	int? x;
	int y;
	def3(y); // #error: The function returns 'int?'
}

fn void test6()
{
	int? x;
	int y;
	def3(x); // #error: The function returns 'int?'
}

fn void test7()
{
	int y;
	def3(y) + 4; // #error: An optional value may not be discarded
}
