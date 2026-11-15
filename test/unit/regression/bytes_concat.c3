module bytes_concat @test;

fn void hex_concat()
{
	char[*] a = x"1234" x"5678";
	assert(a == { 0x12, 0x34, 0x56, 0x78 }, "%s", a);
}

fn void base64_single()
{
	char[*] a = b64"SGVsbG8h";
	assert(a == { 'H', 'e', 'l', 'l', 'o', '!' }, "%s", a);
}

fn void base64_concat()
{
	char[*] a = b64"SGVs" b64"bG8=";
	assert(a == { 'H', 'e', 'l', 'l', 'o' }, "%s", a);
}

fn void base64_concat_three()
{
	char[*] a = b64"SGVs" b64"bG8=" b64"IQ==";
	assert(a == { 'H', 'e', 'l', 'l', 'o', '!' }, "%s", a);
}

fn void hex_then_base64()
{
	char[*] a = x"48" b64"ZWxsbw==";
	assert(a == { 'H', 'e', 'l', 'l', 'o' }, "%s", a);
}

fn void base64_then_hex()
{
	char[*] a = b64"SGVsbA==" x"6f";
	assert(a == { 'H', 'e', 'l', 'l', 'o' }, "%s", a);
}
