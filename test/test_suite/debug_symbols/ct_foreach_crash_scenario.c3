enum TokenType
{
	LOOPS
}
macro populate_token_table()
{
	$foreach $tokenType : TokenType::values:
		$tokenType[token] = $tokenType; // #error: 'token' could not be found, did you spell it right?
	$endforeach
}
const TOKEN_TABLE = populate_token_table ();