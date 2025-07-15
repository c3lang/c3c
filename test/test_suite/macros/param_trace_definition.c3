
macro bool @unlikely(bool #value, $probability = 1.0) @builtin
{
	(void)#value;
	return false;
}

fn void test() {}
macro @unlikely_that_this_is_false(#expr) => @unlikely(!(#expr));

fn int main()
{
    char* op = &&'a';
    uint ctrl = 7; ++ctrl;

    char[] some_slice = "abcdefg";

    char* ref = op - ((ctrl & 31) << 8) - 1;

    if (@unlikely_that_this_is_false(null < ref)) test(); // #error: conversion requires an explicit cast
    test();
    return 0;
}