import std;

macro something() {
	unreachable("Hi there");
}

fn void main() {
	something();

	$echo "Hello";
	io::printn("Hello, World"); // #warning: This code will never execute
}