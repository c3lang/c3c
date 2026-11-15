module test;

// The same for globals and constants. This is a separate file because an error in a global
// declaration stops the analysis of the function bodies, which would hide the local cases.
char[4] g_empty = "";
const char[4] C_EMPTY = "";

// The constant really is four zero bytes.
fn void constant_has_the_array_length()
{
	$assert(C_EMPTY.len == 4);
	$assert(C_EMPTY[0] == 0);
	$assert(C_EMPTY[3] == 0);
}
