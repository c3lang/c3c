struct Object
{
	typeid type;
}

macro Object.to_value(&self, $Type = ...)
{
	$if !$defined($Type):
		var $Type = self.type;
	$endif
}

fn int main()
{
	Object o = {  };
	$Typeof(o.to_value) x; // #error: A macro has no defined type
}