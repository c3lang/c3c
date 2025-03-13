module test;
import std;
alias ListStr = List{String};
fn void test()
{
	List<String> a = List<...>.new_init(); // #error: An expression was expected
}

fn void test2()
{
	List{String} a = List<...>.new_init(); // #error: An expression was expected
}

fn void test3()
{
	List{String} a = List{...}.new_init(); // #error: An expression was expected
}

fn void main()
{
    List<String> a = List<String>.new_init(); // #error: An expression was expected
}