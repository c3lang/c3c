module test;

const char[8] NAME = "abc";

// A constant string converted to a longer array really is that long, padded with zeros.
// Each check is in its own function so that one failure doesn't hide the others.
fn void index_in_string()
{
	$assert(NAME[2] == 'c');
}

fn void index_first_padding()
{
	$assert(NAME[3] == 0);
}

fn void index_last_padding()
{
	$assert(NAME[7] == 0);
}

fn void length()
{
	$assert(NAME.len == 8);
}
