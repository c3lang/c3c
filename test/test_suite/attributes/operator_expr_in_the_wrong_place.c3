fn int main()
{
	int a @if(==); // #error: Expected a boolean compile time constant
}