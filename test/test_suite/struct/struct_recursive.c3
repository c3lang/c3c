struct GlobalNode
{
	Ast node;
}

struct Ast
{
	GlobalNode global; // #error: Recursive defintion of 'GlobalNode'
}