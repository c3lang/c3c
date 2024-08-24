module test;
fn void main() {}

module test(<Type>); // #error: generic
fn void test() {
	Type a;
}