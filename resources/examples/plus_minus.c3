module test;
import std::io;
import libc;

fault TokenResult
{
	NO_MORE_TOKENS
}

fn char[]! read_next(char[]* remaining)
{
	// Skip spaces.
	while (remaining.len > 0 && (*remaining)[0] == ' ') *remaining = (*remaining)[1..];
	char* ptr_start = remaining.ptr;
	usize len = 0;
	while (remaining.len > 0 && (*remaining)[0] != ' ')
	{
		len++;
		*remaining = (*remaining)[1..];
	}
	if (!len) return TokenResult.NO_MORE_TOKENS!;
	return ptr_start[:len];
}

fn void main(char[][] args)
{
	String s = io::stdin().getline();
	defer s.destroy();
	char[] numbers = s.str();
	int val = 0;
	bool add = true;
	while (try char[] token = read_next(&numbers))
	{
		int i = libc::atoi(token.ptr);
		val = add ? val + i : val - i;
		char[]! op = read_next(&numbers);
		if (catch op) break;
		add = op[0] == '+';
	}
	io::printfln("%d", val);
}