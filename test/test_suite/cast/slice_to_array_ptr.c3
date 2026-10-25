module test;

struct Parent { int a; }
struct Child { inline Parent p; int b; }

fn void ok(int[] s, Child[] children)
{
	int[4] x;
	int[3]* a = x[:3];
	int[3]* b = s[1..3];
	int[3]* c = (int[3]*)s;
	int[3]* d = (int[3]*)x[:2];
	int* e = x[:2];
	int* f = s;
	Parent* g = children;
}

fn void too_short()
{
	int[4] x;
	int[3]* a = x[:2]; // #error: Implicitly casting 'int[]' to 'int[3]*' is not permitted
}

fn void too_long()
{
	int[4] x;
	int[3]* a = x[:4]; // #error: Implicitly casting 'int[]' to 'int[3]*' is not permitted
}

fn void unknown_length(int[] s)
{
	int[3]* a = s; // #error: Implicitly casting 'int[]' to 'int[3]*' is not permitted
}

fn void wrong_element()
{
	int[4] x;
	float[3]* a = x[:3]; // #error: It is not possible to cast 'int[]' to 'float[3]*'
}

fn void wrong_nesting()
{
	int[4] x;
	int[3][3]* a = x[:3]; // #error: It is not possible to cast 'int[]' to 'int[3][3]*'
}

fn void struct_array_stride(Child[] children)
{
	Parent[2]* a = children[:2]; // #error: It is not possible to cast 'Child[]' to 'Parent[2]*'
}

fn void no_downcast(Parent[] parents)
{
	Child* a = parents; // #error: It is not possible to cast 'Parent[]' to 'Child*'
}
