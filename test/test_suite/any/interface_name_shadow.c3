import std::io;

interface Abc
{
	fn void type(); // #error: would shadow the built-in property '.type'
}

interface Bcd
{
	fn void ptr(); // #error: would shadow the built-in property '.ptr'
}

fn void main()
{
}
