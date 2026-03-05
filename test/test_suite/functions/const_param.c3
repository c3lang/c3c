import std;
fn int bar(const int* baz) // #error: 'const' is not allowed here, did you try to make a C style const parameter?
{}