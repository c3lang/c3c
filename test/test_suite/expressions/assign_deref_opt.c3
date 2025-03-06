module test;

fn void main()
{
	int? a;
	int*? b;
	*b = a; // #error: You cannot assign to a dereferenced optional
}

