import std::io;

macro convert_string_to_hex(String s)
{
	io::printfn("%02hx", (char[])s);
}

fn void main() {
	char[3] x = x"0305ff";
	io::printfn("%02h", x);
	convert_string_to_hex("colors");
}
