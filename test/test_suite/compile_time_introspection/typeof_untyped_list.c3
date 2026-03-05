fn void a()
{
	typeid a = $typeof({}); // #error:  You cannot take the typeid of a compile time type
}