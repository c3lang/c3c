struct Object
{
	typeid type;
}

macro Object.to_value(&self, $Type = ...)
{
	$if !$defined($Type):
		var $Type = self.type; // #error: Expected a type assigned to $Type.
	$endif
}

fn int main()
{
	Object o = { .type = int::typeid };
	(void)o.to_value();
	return 0;
}