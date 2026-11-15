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
	float[3]* a = x[:3]; // #error: Implicitly casting 'int[]' to 'float[3]*' is not permitted
}

fn void wrong_nesting()
{
	int[4] x;
	int[3][3]* a = x[:3]; // #error: Implicitly casting 'int[]' to 'int[3][3]*' is not permitted
}

fn void struct_array_stride(Child[] children)
{
	Parent[2]* a = children[:2]; // #error: Implicitly casting 'Child[]' to 'Parent[2]*' is not permitted
}

fn void no_downcast(Parent[] parents)
{
	Child* a = parents; // #error: Implicitly casting 'Parent[]' to 'Child*' is not permitted
}
