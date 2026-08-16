struct GlobalNode // #error: Recursive definition of 'GlobalNode'
{
	Ast node;
}

struct Ast
{
	GlobalNode global;
}
