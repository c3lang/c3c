
enum MyEnum : int
{
	V1,
}

fn int toInt(MyEnum value)
{
	return (int)value;
}

fn MyEnum toEnum(int value)
{
	return (MyEnum)value;
}

