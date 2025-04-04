
enum MyEnum : int
{
	V1,
}

fn int toInt(MyEnum value)
{
	return (int)value; // #error: but you can use .ordinal
}

fn MyEnum toEnum(int value)
{
	return (MyEnum)value; // #error: but you can use .from_ordinal()
}

