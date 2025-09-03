struct GlobalNode
{
	Ast node;
}

struct Ast
{
	GlobalNode global; // #error: Recursive definition of 'GlobalNode'
}
