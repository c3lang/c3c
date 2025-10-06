bitstruct Test : char
{
	char type : 0..2;
}

const TEST = ((Test)char.max).type; // #error: This expression cannot be evaluated at compile

fn int main() => 0;