fn void main()
{
	@foo(;int &foo) // #error: did you mean 'int*'
	{
	};
}

macro void @foo(;@body(&foo))
{
}

