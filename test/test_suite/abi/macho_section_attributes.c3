// #target: x64-darwin

int foo @section("foo,   12345678901234567      "); // #error: Mach-o requires the section to be at the most 16 characters
int bar @section("foo,         "); // #error: Mach-o requires 'segment,section' as the format, did you type it correctly?
int baz @section("foo"); // #error: Mach-o requires 'segment,section' as the format, did you type it correctly?
int abc @section("foo,b,c,d,e,f"); // #error: Too many parts to the Mach-o section description.
int def @section("foo,b,c,d");

