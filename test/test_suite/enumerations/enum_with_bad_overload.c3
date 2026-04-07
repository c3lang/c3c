enum Bar
{
	ABC
}

fn sz Bar.len(&self) @operator(len) // #error: Enums already have a .len property, you can't use '@operator(len)' on them
{
	return 1;
}

fn bool Bar.eq(&self, Bar b) @operator(==) // #error: Enums do not support overloading the '==' operator.
{
	return true;
}

fn bool Bar.less(&self, Bar b) @operator(<) // #error: Enums do not support overloading the '<' operator.
{
	return true;
}