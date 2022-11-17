module test;
import std::io;
import libc;

fault TokenResult
{
	NO_MORE_TOKENS
}


// While we could have written this with libc
// the C way, let's showcase some features added to C3.
fn void main(char[][] args)
{
	// Grab a string from stdin
	String s = io::stdin().getline();
	// Delete it at scope end [defer]
	defer s.destroy();

	// Grab the string as a slice.
	char[] numbers = s.str();

	// Track our current value
	int val = 0;

	// Is the current state an add?
	bool add = true;

	while (try char[] token = read_next(&numbers))
	{
		// We're assuming well formed input here
		// so just use atoi.
		int i = libc::atoi(token.ptr);

		// This value is either added or subtracted from the sum.
		val = add ? val + i : val - i;

		// Read an optional token.
		char[]! op = read_next(&numbers);

		// If it's an error, then we're done.
		if (catch op) break;

		// Let's process the operator
		switch (op)
		{
			case "+":
				add = true;
			case "-":
				add = false;
			default:
				io::println("Failed to parse expression.");
				return;
		}
	}
	io::printfln("%d", val);
}

fn char[]! read_next(char[]* remaining)
{
	while (remaining.len > 0 && (*remaining)[0] == ' ')
	{
		// Make the slice smaller
		*remaining = (*remaining)[1..];
	}

	// Store the beginning of the parse
	char* ptr_start = remaining.ptr;
	usz len = 0;
	while (remaining.len > 0 && (*remaining)[0] != ' ')
	{
		// Increase length
		len++;

		// And make the slice smaller
		*remaining = (*remaining)[1..];
	}

	// If it's a zero length token, return an optional result.
	if (!len) return TokenResult.NO_MORE_TOKENS!;

	// Otherwise create a slice from the pointer start and length.
	return ptr_start[:len];
}
