// #target: elf-riscv32
module test;

fn void test1()
{
	asm
	{
		andi $s1, $s2, -2049; // #error: 'andi' expected 'short' limited to 12 bits.
	}
}

fn void test2()
{
	asm
	{
		ori $t0, $t1, 2048; // #error: 'ori' expected 'short' limited to 12 bits.
	}
}

fn void test3()
{
	asm
	{
		slli $a0, $a1, 32u; // #error: 'slli' expected 'char' limited to 5 bits.
	}
}

fn void test4()
{
	asm
	{
		lui $a0, 0xFFFFFF; // #error: 'lui' expected 'uint' limited to 20 bits.
	}
}

fn void test5()
{
	asm
	{
		lui $a0, -1; // #error: 'lui' expected 'uint' limited to 20 bits.
	}
}

fn void test6()
{
	asm
	{
		auipc $a0, -1; // #error: 'auipc' expected 'uint' limited to 20 bits.
	}
}

fn void test7()
{
	asm
	{
		slli $a0, 1, 31u; // #error: 'slli' does not support a direct integer constant here.
	}
}
