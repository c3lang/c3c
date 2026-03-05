struct Token
{
	char kind;
}

macro getTokens() @const
{
	Token[64] $tokes;
	$for var $i = 0; $i < 64; $i++:
		$tokes = ((Token) {$i});
	$endfor
	return $tokes[0..32]; // #error: to slice an expression
}

macro checkTokes() @const
{
	Token[] $tokes = getTokens();
	$for var $i = 0; $i < $tokes.len; $i++:
		$echo $tokes[$i];
	$endfor
}

macro int main2()
{
	checkTokes();
}

fn int main() => checkTokes();
