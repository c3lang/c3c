module std::test;

enum Abc { ABC }

fn void testb()
{
	@sizeof(&$$veccompeq); // #error: You cannot take the address of a builtin function.
	@sizeof(&Abc::from_ordinal); // #error: You cannot take the address of a type function.
}