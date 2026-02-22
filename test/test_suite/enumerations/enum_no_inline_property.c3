module test;
constdef MyEnum : inline short
{
	ITEM1,
	ITEM2,
}

fn int main(String[] args)
{
	$typefrom("MyEnum").oops;  // #error: No method or inner struct/union 'MyEnum.oops'
	return 0;
}