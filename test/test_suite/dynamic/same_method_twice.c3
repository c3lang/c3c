module test;
import std::io;

protocol Abc
{
	fn void test();
}

fn void Abc.hello(&self) {}
fn void Abc.hello(&self) {} // #error: This method is already defined

fn void main() {}